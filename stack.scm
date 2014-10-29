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
  (let ((l (vector-length v)))
    (let copy ((i 0))
      (when (< i l)
	(vector-set! stack i (vector-ref v i))
	(copy (1+ i))))
    l))
