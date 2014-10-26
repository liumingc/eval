
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

;;; (fn (x) (fn (y) (+ x y)))
;;; -> (fn (x) (fn (y) (+ (1 . 0) (0 . 0)))
(define comp-lookup
  (lambda (var e)
    (let nextrib ((e e) (rib 0))
      (let nextelt ((vars (car e)) (elt 0))
	(cond
	 ((null? vars) (nextrib (cdr e) (+ rib 1)))
	 ((eq? var (car vars)) (cons rib elt))
	 (else (nextelt (cdr vars) (+ elt 1))))))))


(define env '())


;;; vm registers(5)
;;;   a accumulator
;;;   x program counter
;;;   e environment
;;;   r rib, arg vals
;;;   s stack

;;; asm code(12)
;;; (halt)
;;; (refer var x)
;;; (assign var x)
;;; (const obj x)
;;; (close vars body x) creates a closure from body, vars and
;;; the current environment, places the closure into the accu-
;;; mulator, and sets the next exp to x.
;;; (test then else)
;;; (conti x) creates a continuation from the current stack,
;;; places this continuation in the accumulator, and sets next to x.
;;; (nuate s var) restores s to be the current stack, sets the
;;; accumulator to the value of var in the current env, and sets
;;; the next exp to (return)
;;; (frame ret x)             --- add frame
;;; (arg x)              --- rib <- (cons reg[a] rib)
;;; (apply)
;;; (return)
;;; + (read)
;;; + (write)


;;; compile, name comp to avoid name conflict.
(define comp
  (lambda (x e next)
    (cond
     ((symbol? x)
      `(refer ,(comp-lookup x e) ,next))
     ((pair? x)
      (record-case x
		   (quote (obj)
			  `(const ,obj ,next))
		   (lambda (vars . body)
		     (list 'close (comp `(begin ,@body) (extend e vars) '(return)) next))
		   (if (test then else)
		       (comp test e
			     `(test
			       ,(comp then e next)
			       ,(comp else e next))))
		   (set! (var val)
			 (comp val e `(assign ,(comp-lookup var e) ,next)))
		   (call/cc (x)
			    (let ((c `(conti
				       (arg
					,(comp x e '(apply))))))
			      (if (tail? next)
				  c
				  `(frame ,next ,c))))
		   (begin exps
			  (let recur ((exps exps))
			    (cond
			     ((null? (cdr exps))
			      (comp (car exps) e next))
			     (else
			      (comp (car exps) e (recur (cdr exps)))))))
		   (let (binds . exps)
		     (comp `((lambda ,(map car binds)
			       ,@exps) ,@(map cadr binds)) e next))
		   (let* (binds . exps)
		     (comp `((lambda ,(reverse (map car binds))
			       ,@exps) ,@(reverse (map cadr binds))) e next))
		   (read ()
			 (list 'read next))
		   (write (obj)
			  (list 'write obj next))
		   (t
		    (let loop ((args (cdr x))
			       (c (comp (car x) e '(apply))))
		      (if (null? args)
			  (if (tail? next)
			      c
			      `(frame ,next ,c))
			  (loop (cdr args)
				(comp (car args)
				      e
				      `(arg ,c))))))))
     (else
      `(const ,x ,next)))))

(define tail?
  (lambda (x)
    (eq? (car x) 'return)))

(define decode
  (let ((ninst 0))
    (lambda (x)
      (set! ninst (+ ninst 1))
      (display "code$ ")
      (case (car x)
	((close test conti nuate frame arg apply return read)
	 (print (car x)))
	((halt)
	 (print (car x))
	 (format #t "~a instructions~%" ninst))
	((const refer assign write)
	 (format #t "~a ~a~%" (list-ref x 0) (list-ref x 1)))))))


;;; diff: frame, ret
(define VM
  (lambda (a x e r s)
    (when *debug*
	  #;(print x)
	  (decode x))
    (record-case x
		 (halt () a)
		 (refer (access x)
			(VM (car (lookup access e)) x e r s))
		 (const (obj x)
			(VM obj x e r s))
		 (close (body x)
			(VM (closure body e) x e r s))
		 (test (then else)
		       (VM a (if a then else) e r s))
		 (assign (access x)
			 (set-car! (lookup access e) a)
			 (VM a x e r s))
		 (conti (x)
			(VM (continuation s) x e r s))
		 (nuate (s var)
			(VM (car (lookup var e)) '(return) e r s))
		 (frame (ret x)
			(VM a x e '() (push ret (push e (push r s)))))
		 (arg (x)
		      (VM a x e (cons a r) s))
		 (apply ()
			(cond
			 ((procedure? a)
			  (record (x e rib s) s
				  (VM (apply a r) x e rib s)))
			 (else
			  (record (body e) a
				  (VM a body (extend e r) '() s)))))
		 (return ()
			 (VM a (index s 0) (index s 1) (index s 2) (- s 3)))
		 (read (x)
		       (VM (read) x e r s))
		 (write (obj x)
			(display obj)
			(VM a x e r s))
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
      (VM '() code env '() 0))))

