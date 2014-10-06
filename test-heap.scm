(use-modules (ice-9 format))
#;(use-modules (ice-9 pretty-print))

(load "heap-v1.scm")


(define test-data
  '((3 3)
    ((quote (3 4)) (3 4))
    ("hello" "hello")
    ((+ 2 3) 5)
    ((cons 'a 'b) (a . b))
    ((let ((x 3)
	   (y 4)
	   (z 5))
       (set! x (+ x z))
       (* x y)) 32)

    ((let ((x (+ 2 3))
	   (y (+ 3 4)))
       (cons x y)) (5 . 7))

    ((call/cc (lambda (k)
		(begin
		  (k 5)
		  (+ 3 4)))) 5)

    ((lambda (x) x) "#<procedure>")
    (((lambda (x) x) 5) 5)
    ((if #f "true" "false") "false")
    ((if #t "true" "false") "true")
    ((if 0 "true" "false") "true")
    ))

(define test
  (lambda (exp res)
    (let ((x (go exp)))
      (if (equal? x res)
	  (begin
	    (format #t "~a -- passed~%" exp)
	    #t)
	  (begin
	    (format #t "~a -- failed~%" exp)
	    #f)))))

(define test-case
  (lambda ()
    (let ((tot 0)
	  (passed 0))
      (for-each
       (lambda (x)
	 ;(record (exp res) x
		 (set! tot (+ tot 1))
		 (if (test (car x) (cadr x))
		     (set! passed (+ passed 1)))
		 ;)
		 )
      test-data)
      (format #t " ---~%passed/total: ~a/~a~%"
	      passed tot))))


(format #t "ello~%")
(test-case)
