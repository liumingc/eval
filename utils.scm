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

