
(load "utils.scm")

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
