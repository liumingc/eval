;;; common utilities
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

(load "utils.scm")


;;; functional can't be called outside it's lex-scope
(define functional
  (lambda (body e)
    (list body e)))

;;; stack
(define stack (make-vector 8000))

(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))

;;; (index sp 0) -> pc (index sp 1) -> arg0 ...
(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))

(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))

(define comp
  (lambda (x e next)
    (cond
     ((symbol? x)
      (comp-lookup x e
		   (lambda (n m)
		     (list 'refer n m next))))
     ((pair? x)
      (record-case x
		   (quote (obj)
			  (list 'const obj next))
		   (lambda (vars body)
		     (list 'close
			   (comp body
				 (extend e vars)
				 (list 'return (+ (length vars) 1)))
			   next))
		   (if (test then else)
		       (let ((thenc (comp then e next))
			     (elsec (comp else e next)))
			 (comp test e `(test ,thenc ,elsec))))
		   (set! (var x)
			 (comp-lookup var e
				      (lambda (n m)
					(comp x e `(assign ,n ,m ,next)))))
		   (t (let f ((args (cdr x))
			      (c (comp (car x) e '(apply))))
			(if (null? args)
			    (list 'frame next c)
			    (f (cdr args)
			       (comp (car args) e (list 'arg c))))))))
     (else (list 'const x next)))))

(define comp-lookup
  (lambda (var e fn)
    (let nextrib ((e e) (rib 0))
      (let nextelt ((vars (car e))
		    (elt 0))
	(cond
	 ((null? vars) (nextrib (cdr e) (+ rib 1)))
	 ((eq? var (car vars)) (fn rib elt))
	 (else (nextelt (cdr vars) (+ elt 1))))))))

(define extend
  (lambda (e r)
    (cons r e)))

;;; link is env link, static link
(define VM
  (lambda (a x e s)
    (record-case x
		 (halt () a)
		 (refer (n m x)
			(VM (index (find-link n e) m) x e s))
		 (const (obj x)
			(VM obj x e s))
		 (assign (n m x)
			 (VM (index-set! (find-link n e) m a) x e s))
		 (test (yes no)
		       (VM a (if a yes no) e s))
		 (close (body x)
			(VM (functional body e) x e s))
		 (frame (ret x)
			(VM a x e (push ret (push e s))))
		 (arg (x)
		      (VM a x e (push a s)))
		 (apply ()
			(record (body link) a
				(VM a body s (push link s))))
		 (return (n)
			 (let ((s (- s n)))
			   (VM a (index s 0) (index s 1) (- s 2)))))))


(define find-link
  (lambda (n e)
    (if (= n 0)
	e
	(find-link (- n 1) (index e -1)))))

(define evaluate
  (lambda (x)
    (VM '() (comp x '() '(halt)) 0 0)))
