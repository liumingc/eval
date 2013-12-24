#lang racket

;; (require compatibility/mlist)

#|
env is
(env
  (a 3) (b 4) (c 5) ...)
|#

;;; env ;;;
(define env0 (mcons 'env '()))

(define ext-env
  (lambda (e0 var val)
    (set-mcdr! e0
               (mcons (mcons var val) (mcdr e0)))))

(define set-var
  (lambda (e0 var val)
    (let ([p (lookup0 e0 var)])
      (if (not (null? p))
        (begin
          (set-mcdr! p val)
          (printf "set ~a to ~a~n" var val))
        (begin
          (printf "set ~a failed~n" var)
          (void))))))

(define show-env
  (lambda (e0)
    (letrec ([dat (mcdr e0)]
             [f (lambda (x)
                  (if (null? x)
                    (void)
                    (begin
                      (display (mcar x))
                      (newline)
                      (f (mcdr x)))))])
      (f dat))))

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

;;; lambda ;;;



;;; evaluator ;;;
#|
(define basic-form
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
    (usr-print (eval (read) env0))
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

(define eval
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
       (nop expr)]
      ;; make print as built-in, to make debug easier
      ;; print behaves different with scheme's print
      [(print? expr)
       (parse-print expr)]
      [else ;; fn-call ...
        (nop expr)])))

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
    (lambda (env)
      (let ([var (list-ref expr 1)]
            [val (eval (list-ref expr 2) env)])
        (ext-env env var val)))))

(define parse-set
  (lambda (expr)
    (lambda (env)
      (let ([var (list-ref expr 1)]
            [val (eval (list-ref expr 2) env)])
        (set-var env var val)))))

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
      (letrec ([f (lambda (exprs)
                    (if (null? exprs)
                      (lambda (env) (void)) ;;; todo
                      (lambda (env)
                        ((car exprs) env)
                        ((f (cdr exprs)) env))))])
        (lambda (env)
          ((f exprs) env))))))

(define parse-print
  (lambda (expr)
    (lambda (env)
      (map display (cdr expr))
      (void))))

;;; test ;;;
#|
(ext-env env0 'a 3)
(ext-env env0 'b 4)
(ext-env env0 'c 5)

(display (lookup env0 'c))
(newline)
(display (lookup env0 'd))
(set-var env0 'a 7)
(show-env env0)
|#
(repl)
