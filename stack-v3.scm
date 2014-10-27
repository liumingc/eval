;;; stack-alloc static chain, that's envirenment.

(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

(load "utils.scm")

(define *debug* #f)


;;; env utilities
;;; ((vars . vals) env-link)


(define extend
  (lambda (env vars)
    (cons vars env)))

(define lookup
  (lambda (access e)
    (let ((vars (list-ref e (car access))))
      (let f ((r (car e))
	      (elt (cdr access)))
	(cond
	 ((zero? elt) r)
	 (else (f (cdr r) (- elt 1))))))))


(define comp-lookup
  (lambda (var e fn)
    (let nextrib ((e e) (rib 0))
      (let nextelt ((vars (car e))
		    (elt 0))
	(cond
	 ((null? vars) (nextrib (cdr e) (+ rib 1)))
	 ((eq? var (car vars)) (fn rib elt))
	 (else (nextelt (cdr vars) (+ elt 1))))))))


(define env '())


;;; compile, name comp to avoid name conflict.
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
				 (list 'return (1+ (length vars))))
			   next))
		   (if (test then else)
		       (let ((thenpart (comp then e next))
			     (elsepart (comp else e next)))
			 (comp test e (list 'test thenpart elsepart))))
		   (call/cc (x)
			    (list 'frame
				  next
				  (list 'conti
					(list 'arg
					      (comp x e '(apply))))))
		   (t
		    (let loop ((args (cdr x))
			       (c (comp (car x) e '(apply))))
		      (if (null? args)
			  `(frame ,next ,c)
			  (loop (cdr args)
				(comp (car args)
				      e
				      `(arg ,c))))))))
     (else
      (list 'const x next)))))

(define (decode inst)
  (let ((op (car inst)))
    (format #t "$ ~s~%" op)))

;;; rib is in the stack
(define VM
  (lambda (a x e s)
    (if *debug*
	(decode x))
    (record-case x
		 (halt () a)
		 (refer (n m x)
			(VM (index (find-link n e) m) x e s))
		 (const (obj x)
			(VM obj x e s))
		 (close (body x)
			(VM (closure body e) x e s))
		 (test (then else)
		       (VM a (if a then else) e s))
		 (conti (x)
			(VM (continuation s) x e s))
		 (nuate (s var)
			(VM (index (find-link n e) m) '(return) e (restore-stack s)))
		 (frame (ret x)
			(VM a x e (push ret (push e s))))
		 (arg (x)
		      (VM a x e (push a s)))
		 (apply ()
			(record (body e) a
				(VM a body s (push e s))))
		 (return (n)
			 (let ((s (- s n)))
			   (VM a (index s 0) (index s 1) (- s 2))))
		 )))


(define closure
  (lambda (body e)
    (list body e)))


(define continuation
  (lambda (s)
    (closure
     (list 'refer 0 0 (list 'nuate (save-stack s) '(return)))
     '())))

;;; stack utils
(define stack-sz 2048)
(define stack (make-vector stack-sz))

(define (push val s)
  (vector-set! stack s val)
  (1+ s))

(define index
  (lambda (s i)
    (vector-ref stack (1- (- s i)))))

(define index-set!
  (lambda (s i v)
    (vector-set! stack (1- (- s i)) v)))

;;; continuation has to copy the whole stack
(define (save-stack s)
  (let ((v (make-vector stack-sz)))
    (let copy ((i 0))
      (when (< i s)
	(vector-set! v i (vector-ref s i))
	(copy (1+ i))))
    v))

(define (restore-stack v)
  (let ((l (vetor-length v)))
    (let copy ((i 0))
      (when (< i l)
	(vector-set! stack i (vector-ref v i))
	(copy (1+ i))))
    l))


(define go
  (lambda (x)
    (let ((code (comp x '() '(halt))))
      (if *debug*
	  (pretty-print code))
      (VM '() code env 0))))

(define find-link
  (lambda (n e)
    (cond
     ((= n 0) e)
     (else (find-link (1- n) (index e -1))))))
