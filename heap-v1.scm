;;; add primitive proc
;;; add begin, let form
(define *debug* #f)


(define print
  (lambda (x)
    (display x)
    (newline)))

(define-syntax recur
  (syntax-rules ()
    ((_ f ((v e) ...) b ...)
     (let ((f #f))
       (set! f
	     (lambda (v ...) b ...))
       (f e ...)))))

;;; destructuring-bind in CL
(define-syntax record
  (syntax-rules ()
    ((_ fields var body ...)
     (apply (lambda fields body ...) var))))



(define-syntax record-case
  (lambda (exp)
    (syntax-case exp ()
      ((_ exp c1 c2 ...)
       #`(let ((x exp))
	   (cond
	    #,@(let f ((c1 #'c1)
		       (c2 #'(c2 ...)))
		 (if (null? c2)
		     (syntax-case c1 (t) ; t as else
		       ((t body ...)
			#'((else body ...)))
		       ((k fields body ...)
			#'(((eqv? (car x) 'k)
			    (record fields (cdr x) body ...)))))
		     (syntax-case c1 ()
		       ((k fields body ...)
			#`(((eqv? (car x) 'k)
			    (record fields (cdr x) body ...))
			   #,@(f (car c2) (cdr c2)))))))))))))





;;; env utilities
;;; ((vars . vals) env-link)


(define extend
  (lambda (env vars vals)
    (cons (cons vars vals) env)))

(define lookup
  (lambda (var e)
    (let nextrib ((e e))
      (if (null? e)
	  (error "var not defined" var))
      (let nextelt ((vars (caar e))
		    (vals (cdar e)))
	(cond
	 ((null? vars) (nextrib (cdr e)))
	 ((eq? (car vars) var) vals)
	 (else (nextelt (cdr vars) (cdr vals))))))))



(define env '())

(define init
  (lambda ()
    (set! env
	  (extend env
		  '(+ - * / cons car cdr)
		  (list + - * / cons car cdr)))))

(init)


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
;;; + (call) calls primitive procedure


;;; compile, name comp to avoid name conflict.
(define comp
  (lambda (x next)
    (cond
     ((symbol? x)
      `(refer ,x ,next))
     ((pair? x)
      (record-case x
		   (quote (obj)
			  `(const ,obj ,next))
		   (lambda (vars body)
		     `(close ,vars ,(comp body '(return)) ,next))
		   (if (test then else)
		       (comp test
			     `(test
			       ,(comp then next)
			       ,(comp else next))))
		   (set! (var val)
			 (comp val `(assign ,var ,next)))
		   (call/cc (x)
			    (let ((c `(conti
				       (arg
					,(comp x '(apply))))))
			      (if (tail? next)
				  c
				  `(frame ,next ,c))))
		   (begin exps
			  (let recur ((exps exps))
			    (cond
			     ((null? (cdr exps))
			      (comp (car exps) next))
			     (else
			      (comp (car exps) (recur (cdr exps)))))))
		   (let (binds . exps)
		     (comp `((lambda ,(map car binds)
			       (begin ,@exps)) ,@(map cadr binds)) next))
		   (t
		    (let loop ((args (cdr x))
			       (c (comp (car x) '(apply))))
		      (if (null? args)
			  (if (tail? next)
			      c
			      `(frame ,next ,c))
			  (loop (cdr args)
				(comp (car args)
				      `(arg ,c))))))))
     (else
      `(const ,x ,next)))))

(define tail?
  (lambda (x)
    (eq? (car x) 'return)))

;;; SECD machine? I don't known semantics.
;;; The dragon book don't mention this.
(define VM
  (lambda (a x e r s)
    (when *debug*
	  (display "code$ ")
	  #;(print x)
	  (print (car x))
	  )
    (record-case x
		 (halt () a)
		 (refer (var x)
			(VM (car (lookup var e)) x e r s))
		 (const (obj x)
			(VM obj x e r s))
		 (close (vars body x)
			(VM (closure vars body e) x e r s))
		 (test (then else)
		       (VM a (if a then else) e r s))
		 (assign (var x)
			 (set-car! (lookup var e) a)
			 (VM a x e r s))
		 (conti (x)
			(VM (continuation s) x e r s))
		 (nuate (s var)
			(VM (car (lookup var e)) '(return) e r s))
		 (frame (ret x)
			(VM a x e '() (call-frame ret e r s)))
		 (arg (x)
		      (VM a x e (cons a r) s))
		 (apply ()
			(cond
			 ((procedure? a)
			  (record (x e rib s) s
				  (VM (apply a r) x e rib s)))
			 (else
			  (record (vars body e) a
				  (VM a body (extend e vars r) '() s)))))
		 (return ()
			 (record (x e r s) s
				 (VM a x e r s))))))


(define closure
  (lambda (vars body e)
    (list vars body e)))


(define continuation
  (lambda (s)
    (closure '(v) (list 'nuate s 'v) '())))

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

(define go
  (lambda (x)
    (VM '() (comp x '(halt)) env '() '())))
