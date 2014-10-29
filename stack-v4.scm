;;; display closure

(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

(load "utils.scm")
(load "stack.scm")

(define *debug* #t)

;;; x -> expr, b -> bound-vars
(define find-free
  (lambda (x b)
    (cond
     ((symbol? x)
      (if (set-member? x b) '() (list x)))
     ((pair? x)
      (record-case x
		   (quote (obj) '())
		   (lambda (vars body)
		     (find-free body (set-union vars b)))
		   (if (test then else)
		       (set-union (find-free test b)
				  (set-union (find-free then b)
					     (find-free else b))))
		   (call/cc (exp) (find-free exp b))
		   (t
		    (let next ((x x))
		      (if (null? x)
			  '()
			  (set-union (find-free (car x) b)
				     (next (cdr x))))))))
     (else '()))))


;;; ==================================================================
;;; set helper funcs
;;; simply, (memq x set)
(define (set-member? x set)
  (cond
   ((null? set) #f)
   ((eq? x (car set)) #t)
   (else (set-member? x (cdr set)))))

(define (set-cons x set)
  (if (set-member? x set)
      set
      (cons x set)))

(define (set-union s1 s2)
  (if (null? s1)
      s2
      (set-union (cdr s1) (set-cons (car s1) s2))))

(define (set-minus s1 s2)
  (cond
   ((null? s1) '())
   ((set-member? (car s1) s2) (set-minus (cdr s1) s2))
   (else (cons (car s1) (set-minus (cdr s1) s2)))))

(define (set-intersect s1 s2)
  (cond
   ((null? s1) '())
   ((set-member? (car s1) s2) (cons (car s1) (set-intersect (cdr s1) s2)))
   (else (set-intersect (cdr s1) s2))))


;;; ==================================================================
(define comp
  (lambda (x e next)
    (cond
     (
      ;; change
      (symbol? x)
      (comp-refer x e next))
     ((pair? x)
      (record-case x
		   (quote (obj)
			  (list 'const obj next))
		   ;; change
		   (lambda (vars body)
		     (let ((free (find-free body vars)))
		       (collect-free free e
				     (list 'close
					   (length free)
					   (comp body
						 (cons vars free)
						 (list 'return (length vars)))
					   next))))
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


(define collect-free
  (lambda (vars e next)
    (if (null? vars)
	next
	(collect-free (cdr vars) e
		      (comp-refer (car vars) e
				  (list 'arg next))))))

(define comp-refer
  (lambda (x e next)
    (comp-lookup x e
		 (lambda (n) (list 'refer-local n next))
		 (lambda (n) (list 'refer-free n next)))))

(define comp-lookup
  (lambda (x e return-local return-free)
    (let nxtlocal ((locals (car e)) (n 0))
      (if (null? locals)
	  (let nxtfree ((frees (cdr e)) (n 0))
	    (if (eq? (car frees) x)
		(return-free n)
		(nxtfree (cdr frees) (1+ n))))
	  (if (eq? (car locals) x)
	      (return-local n)
	      (nxtlocal (cdr locals) (1+ n)))))))


;;; ==================================================================
;;; register change. c -> closure-vars, f -> frame
;;; too new instructions: refer-local, refer-free
(define (decode inst)
  (let ((op (car inst)))
    (format #t "$ ~s~%" op)))

(define VM
  (lambda (a x f c s)
    (if *debug*
	(decode x))
    (record-case x
		 (halt () a)
		 (refer-local (n x)
			      (VM (index f n) x f c s))
		 (refer-free (n x)
			     (VM (index-closure c n) x f c s))
		 (const (obj x)
			(VM obj x f c s))
		 (close (n body x)
			(VM (closure body n s) x f c (- s n)))
		 (test (then else)
		       (VM a (if a then else) f c s))
		 (conti (x)
			(VM (continuation s) x f c s))
		 (nuate (s x)
			(VM a x f c (restore-stack s))
			;(VM (index (find-link n e) m) '(return) e (restore-stack s))
			)
		 (frame (ret x)
			(VM a x f c (push ret (push f (push c s)))))
		 (arg (x)
		      (VM a x f c (push a s)))
		 (apply ()
			(VM a (closure-body a) s a s))
		 (return (n)
			 (let ((s (- s n)))
			   (VM a (index s 0) (index s 1) (index s 2) (- s 3))))
		 )))

(define closure
  (lambda (body n s)
    (let ((v (make-vector (1+ n))))
      (vector-set! v 0 body)
      (let f ((i 0))
	(unless (= i n)
	  (vector-set! v (+1 i) (index s i))
	  (f (1+ i))))
      v)))

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

(define index-closure
  (lambda (c n)
    (vector-ref c (1+ n))))


(define continuation
  (lambda (s)
    (closure
     (list 'refer-local 0 (list 'nuate (save-stack s) '(return 0)))
     0
     0)))

(define go
  (lambda (x)
    (let ((code (comp x '() '(halt))))
      (if *debug*
	  (pretty-print code))
      (VM '() code 0 '() 0))))

