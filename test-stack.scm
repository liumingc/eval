(use-modules (ice-9 format))
#;(use-modules (ice-9 pretty-print))

;;; ifndef *test-file*; then setq *test-file* "stack-v3.scm"; fi
(load "stack-v4.scm")

(define test-data
  '((3 3)
    ((quote (3 4)) (3 4))
    ("hello" "hello")
    ((lambda (x) x) "#<procedure>")
    (((lambda (x) x) 5) 5)
    (((lambda (f x) (f x)) (lambda (x) x) "-func-") "-func-")
    ((if #f "true" "false") "false")
    ((if #t "true" "false") "true")
    ((if 0 "true" "false") "true")
    ((call/cc (lambda (k) (k "conti"))) "conti")
#;
    ((let ((kons (lambda (ka kd)
    (lambda (key)
    (if (eq? key 'kar)
    ka
    kd))))
    (kar (lambda (k)
    (k 'kar)))
    (kdr (lambda (k)
    (k 'kdr))))
    (kar (kons 3 4))) 3)
    ))

(define test
  (lambda (exp res)
    (let ((x (go exp)))
      (if (equal? x res)
	  (begin
	    (format #t "~s -- passed~%" exp)
	    #t)
	  (begin
	    (format #t "~s ~s -- failed~%" exp x)
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


(format #t "hello~%")
(test-case)
