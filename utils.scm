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

#|
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
|#

(define-syntax record-case
  (lambda (x)
    (syntax-case x ()
      ((_ var c1 c2 ...)
       (let f ((cl #'c1)
               (crest #'(c2 ...)))
         (if (null? crest)
             (syntax-case cl (else)
               ((else e1 e2 ...)
                #'(begin e1 e2 ...))
               ((kw (a1 ...) e1 e2 ...)
                #'(if (eq? 'kw (car var))
                      (record (a1 ...) (cdr var) e1 e2 ...))))
             (with-syntax ((rest (f (car crest) (cdr crest))))
                          (syntax-case cl ()
                            ((kw (a1 ...) e1 e2 ...)
                             #'(if (eq? 'kw (car var))
                                   (record (a1 ...) (cdr var) e1 e2 ...)
                                   rest))))))))))

#|
; this version works too. at first i dont know how it works, but now i do.
; inside template, a var is a binding/reference/pattern-variable, otherwise it will signal an error.
(define-syntax record-case
  (lambda (x)
    (define build-clause
      (lambda (x clause clause*)
	(if (null? clause*)
	    (syntax-case clause (else)
	      ((else e1 e2 ...)
	       #'(begin e1 e2 ...))
	      ((kw (arg ...) e1 e2 ...)
	       #'(let ((k (car x)))
		   (if (eq? k 'kw)
		       (apply (lambda (arg ...) e1 e2 ...) (cdr x))
		       (printf "no match ~s" x)))))
	    (with-syntax
	     ([x x] [crest (build-clause x (car clause*) (cdr clause*))])
	     (syntax-case clause (else)
	       ((kw (arg ...) e1 e2 ...)
		#'(let ((k (car x)))
		    (if (eq? k 'kw)
			(apply (lambda (arg ...) e1 e2 ...) (cdr x))
			crest))))))))
    (syntax-case x ()
      [(_ x m1 m2 ...)
       (build-clause #'x #'m1 #'(m2 ...))])))
|#
