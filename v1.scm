
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
	   ;(print x)
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


(define stream-ref
  (lambda (s n)
    (if (= n 0)
	(car s)
	(stream-ref ((cdr s)) (- n 1)))))



(define-syntax coroutine
  (syntax-rules ()
    ((_ var exp ...)
     (define var
       (let ((swt
	      (lambda (next)
					;(display "yield")
		(newline)
		(call/cc
		 (lambda (k)
		   (set! var (lambda () k))
		   (next)   ; at this point, swtch to other coroutines
		   )))))
	 (lambda () exp ...))))))

					;(display "v1 loaded")


;;; meta-circulate-interpreter


(define exec
  (lambda (exp env)
    (cond
     ((symbol? exp) (car (lookup exp env)))
     ((pair? exp)
      (record-case exp
		   (quote (obj) obj)
		   (lambda (vars body)
		     (lambda (vals)
		       (exec body (extend env vars vals))))
		   (if (test then else)
		       (if (exec test env)
			   (exec then env)
			   (exec else env)))
		   (begin exps
			  (let seq ((exps exps))
			    (cond
			     ((null? (cdr exps)) (exec (car exps) env))
			     (else (exec (car exps) env) (seq (cdr exps))))))
		   ;; combine define & set!
		   (define (var val)
		     (let ((rib (car env)))
		       (set-car! rib (cons var (car rib)))
		       (set-cdr! rib (cons val (cdr rib)))))
		   (set! (var val)
			 (set-car! (lookup var env) (exec val env)))
		   (call/cc (exp)
			    (call/cc
			     (lambda (k)
			       ((exec exp env)
				(list (lambda (args) (k (car args))))))))
		   (t
		    ;; application
		    ((exec (car exp) env)
		     (map (lambda (x) (exec x env)) (cdr exp))))))
     (else exp))))


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
