#lang racket

(require compatibility/mlist)

#|
env is
(env
  (a . 3) (b . 4) (c . 5) ...)
|#

(define env0 (mcons 'env '()))

;; (ext-env e0 (a1 a2) (v1 v2))
(define ext-env
  (lambda (e0 params vals)
    (let ([env (mcdr e0)])
      (define (f vars vals)
        (if (not (null? vars))
            (mcons (mcons (car vars)
                          (car vals))
                   (f (cdr vars)
                      (cdr vals)))
            env))
      (mcons 'env (f params vals)))))

(define def-var
  (lambda (e0 var val)
    (set-mcdr! e0
               (mcons (mcons var val) (mcdr e0)))
    (printf "def ~a to ~a~n" var val)
    'def-ok))

(define set-var
  (lambda (e0 var val)
    (let ([p (lookup0 e0 var)])
      (if (not (null? p))
          (begin
            (set-mcdr! p val)
            (printf "set ~a to ~a~n" var val)
            'set-ok)
          (begin
            ;;(printf "set ~a failed~n" var)
            'set-failed)))))

(define show-env
  (lambda (e0)
    (printf ">>> ENV >>>~n")
    (mmap (lambda (x)
            (display x)
            (newline))
          (mcdr e0))
    (printf "<<< ENV <<<~n")))

(define lookup0
  (lambda (e0 var)
    (define f
      (lambda (x)
        (if (null? x)
            (begin
              (printf "lookup ~a failed~n" var)
              '())
            (let ([p (mcar x)])
              (if (equal? (mcar p) var)
                  p
                  (f (mcdr x)))))))
    (f (mcdr e0))))

(define lookup
  (lambda (e0 var)
    (let ([x (lookup0 e0 var)])
      (if (null? x)
          '()
          (mcdr x)))))

;;; evaluator ;;;
#|
(basic-form
  (define var val)
  (set var val)
  (quote dat)
  (if cond then-clause else-clause)
  (begin stmt-list ...)
  (fn (param-list ...) body-list ...)
  (fn-call arg-list ...))
|#

(define repl
  (lambda ()
    (prompt)
    (usr-print (eval. (read) env0))
    (repl)))

(define prompt
  (lambda ()
    (display "% ")))

(define usr-print
  (lambda (expr)
    (if (void? expr)    ;; #<void>, how to compare?
        (begin
          (newline)
          (newline))
        (begin
          (display expr)
          (newline)
          (newline)))))

(define eval.
  (lambda (expr env)
    ((parse expr) env)))

(define parse
  (lambda (expr)
    (cond
      [(atom? expr)
       (parse-atom expr)]
      [(var? expr)
       (parse-var expr)]
      [(define? expr)
       (parse-define expr)]
      [(set? expr)
       (parse-set expr)]
      [(quote? expr)
       (parse-quote expr)]
      [(if? expr)
       (parse-if expr)]
      [(begin? expr)
       (parse-begin expr)]
      [(fn? expr)
       (parse-fn expr)]
      ;; make print as built-in, to make debug easier
      ;; print behaves different with scheme's print
      [(print? expr)
       (parse-print expr)]
      [else ;; fn-call ...
       (parse-fn-call expr)])))

(define atom?
  (lambda (expr)
    (or (string? expr)
        (number? expr)
        (null? expr)
        (eq? expr '#t)
        (eq? expr '#f))))

(define var?
  (lambda (expr)
    (symbol? expr)))

(define match-tag?
  (lambda (expr tag)
    (and (pair? expr)
         (equal? (car expr) tag))))

(define define?
  (lambda (expr)
    (match-tag? expr 'define)))
(define set?
  (lambda (expr)
    (or (match-tag? expr 'set!)
        (match-tag? expr 'set))))
(define quote?
  (lambda (expr)
    (match-tag? expr 'quote)))
(define if?
  (lambda (expr)
    (match-tag? expr 'if)))
(define begin?
  (lambda (expr)
    (match-tag? expr 'begin)))
(define fn?
  (lambda (expr)
    (match-tag? expr 'fn)))
(define print?
  (lambda (expr)
    (match-tag? expr 'print)))
(define fn-call?
  (lambda (expr)
    (pair? expr)))

(define nop
  (lambda (expr)
    (lambda (env)
      (printf "~a not implemented~n" (car expr)))))

(define parse-atom
  (lambda (expr)
    (lambda (env)
      expr)))

(define parse-var
  (lambda (expr)
    (lambda (env)
      (lookup env expr))))

(define parse-define
  (lambda (expr)
    (let ([var (list-ref expr 1)]
          [val (parse (list-ref expr 2))])
      (lambda (env)
        (def-var env var (val env))))))

(define parse-set
  (lambda (expr)
    (let ([var (list-ref expr 1)]
          [val (parse (list-ref expr 2))])
      (lambda (env)
        (set-var env var (val env))))))

(define parse-quote
  (lambda (expr)
    (lambda (env)
      (cadr expr))))

;; (if test-clause then-clause else-clause)
(define parse-if
  (lambda (expr)
    (let ([test-clause (parse (list-ref expr 1))]
          [then-clause (parse (list-ref expr 2))]
          [else-clause (parse (list-ref expr 3))])
      (lambda (env)
        (if (equal? (test-clause env) #f)
            (else-clause env)
            (then-clause env))))))

;; (begin e1 e2 ...)
(define parse-begin
  (lambda (expr)
    (let ([exprs (map parse (cdr expr))])
      (define parse-seq
        (lambda (seqs)
          (if (null? (cdr seqs))
              (car seqs)
              (let ([rest (parse-seq (cdr seqs))])
                (lambda (env)
                  ((car seqs) env)
                  (rest env))))))
      (parse-seq exprs))))

;; (fn (a1 a2 ...) b1 b2 ...)
;; (fn fn-params fn-body fn-env)
(define (parse-fn expr)
  (lambda (env)
    (let ([f (list 'fn (cadr expr) (parse (cons 'begin (cddr expr))) env)])
      f)))

(define (fn-params fn-obj)
  (list-ref fn-obj 1))

(define (fn-body fn-obj)
  (list-ref fn-obj 2))

(define (fn-env fn-obj)
  (list-ref fn-obj 3))

;; (f a b c)
(define parse-fn-call
  (lambda (expr)
    (let ([fp (parse (car expr))]
          [args (map parse (cdr expr))])
      (lambda (env)
        (let ([fo (fp env)])
          ((fn-body fo)
           (ext-env (fn-env fo)
                    (fn-params fo)
                    (map (lambda (x)
                           (x env)) args))))))))

(define parse-print
  (lambda (expr)
    (let ([elems (map (lambda (x)
                        (parse x))
                      (cdr expr))])
      (lambda (env)
        (map (lambda (x)
               (display (x env)))
             elems)
		(void)))))

;;; test ;;;
#|
(def-var env0 'a 3)
(def-var env0 'b 4)
(def-var env0 'c 5)

(display (lookup env0 'c))
(newline)
(display (lookup env0 'd))
(set-var env0 'a 7)
(show-env env0)

(show-env (ext-env env0 '(x y z) '(0 1 2)))
|#
;; cons/car/cdr can be done by:
;; (define cons (fn (x y) (fn (p) (p x y))))
;; (define car (fn (p) (p (fn (x y) x))))
;; (define cdr (fn (p) (p (fn (x y) y))))
(repl)
