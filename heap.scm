(load "utils.scm")

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
	  (extend env '(+ - * / cons car cdr)
		  (map (lambda (op)
			 (lambda (x) (apply op x))) (list + - * / cons car cdr))))))

(init)

(define repl
  (lambda ()
    (display "> ")
    (let ((res (exec (read) env)))
      (display "= ")
      (display res)
      (newline))
    (repl)))


;;; vm registers(5)
;;;   a accumulator
;;;   x program counter
;;;   e environment
;;;   r rib, argument vals
;;;   s stack

;;; asm code(12)
;;; (halt)
;;; (refer var x)
;;; (const obj x)
;;; (close vars body x) creates a closure from body, vars and
;;; the current environment, plases the closure into the accu-
;;; mulator, and sets the next exp to x.
;;; (test then else)
;;; (assign var x)
;;; (conti x) creates a continuation from the current stack,
;;; places this continuation in the accumulator, and sets next to x.
;;; (nuate s var) restores s to be the current stack, sets the
;;; accumulator to the value of var in the current env, and sets
;;; the next exp to (return)
;;; (frame ret x)             --- add frame
;;; (argument x)              --- rib <- (cons reg[a] rib)
;;; (apply)
;;; (return)

(define *debug* #f)

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
		       `(comp ,test
			      ,(comp then next)
			      ,(comp else next)))
		   (set! (var val)
			 (comp val `(assign ,var ,next)))
		   (call/cc (x)
			    (let ((c `(conti
				       (argument
					,(comp x '(apply))))))
			      (if (tail? next)
				  c
				  `(frame ,next ,c))))
		   (t
		    (let loop ((args (cdr x))
			       (c (comp (car x) '(apply))))
		      (if (null? args)
			  (if (tail? next)
			      c
			      `(frame ,next ,c))
			  (loop (cdr args)
				(comp (car args)
				      `(argument ,c))))))))
     (else
      `(const ,x ,next)))))

(define tail?
  (lambda (x)
    (eq? (car x) 'return)))

;;; SECD machine? I don't known semantics.
;;; The dragon book don't mention this.
(define VM
  (lambda (a x e r s)
    (record-case x
		 (halt () a)
		 (refer (var x)
			(VM (car (lookup var e)) x e r s))
		 (const (obj x)
			(VM obj x e r s))
		 (close (vars body x)
			(VM (closure body e vars) x e r s))
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
		 (argument (x)
			   (VM a x e (cons a r) s))
		 (apply ()
			(record (body e vars) a
				(VM a body (extend e vars r) '() s)))
		 (return ()
			 (record (x e r s) s
				 (VM a x e r s))))))


(define closure
  (lambda (body e vars)
    (list body e vars)))

(define continuation
  (lambda (s)
    (closure (list 'nuate s 'v) '() '(v))))

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

(define go
  (lambda (x)
    (VM '() (comp x '(halt)) '() '() '())))

