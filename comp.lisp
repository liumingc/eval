;; (defvar sym val)
;; (defparameter sym val)
;; for defparameter, val is always evaluated.
;; for defvar, val is evaluated only when sym is unbound.
(defparameter *debug-flag* t)

(defun dbg (&rest args)
  (when *debug-flag*
    (apply #'format t args)))

(set-dispatch-macro-character #\# #\;
                              #'(lambda (s ch n)
                                  (declare (ignore ch n))
                                  (read s nil nil t)
                                  nil))

(defstruct (inst
             (:constructor make-inst (need mod inst))
             (:print-object print-inst))
  need
  mod
  inst)

(defun print-inst (o s)
  (let ((*print-pretty* t))
    (format s "~a, ~a~%" (inst-need o) (inst-mod o))
    (dolist (x (inst-inst o))
      (format s "~a~%" x))))

(defun compile-linkage (link)
  (cond
    ((eq link 'return)
     (make-inst '(continue) '()
                `((goto (reg continue)))))
    ((eq link 'next)
     (make-empty-instruction))
    (t                               ;label
     (make-inst '() '()
                `((goto (label ,link)))))
    ))

(defun make-empty-instruction ()
  (make-inst () () ()))

(defun set-union (&rest args)
  (labels ((set-u2 (s1 s2)
             (cond ((null s1) s2)
                   ((memq (car s1) s2) (set-u2 (cdr s1) s2))
                   (t (set-u2 (cdr s1) (cons (car s1) s2))))))
    (let ((na (mapcar #'(lambda (x)
                          (set-u2 x nil)) args)))
      (reduce #'set-u2 na :initial-value nil))))

(defun set-diff (s1 s2)
  (cond ((null s1) nil)
        ((memq (car s1) s2) (set-diff (cdr s1) s2))
        (t (cons (car s1) (set-diff (cdr s1) s2)))))


(defun set-intersect (&rest args)
  (let ((elts (apply #'set-union args)))
    (do ((s elts (cdr s))
         (ans nil))
        ((null s) ans)
      (if (every #'(lambda (x) (memq (car s) x)) args)
          (setq ans (cons (car s) ans))))))


;; 5.5.4 combinition of instrutions
;;; tocheck
#;(defun preserving (reg seq1 seq2)
    (let ((regs (set-intersect reg (inst-mod seq1) (inst-need seq2))))
      (make-inst (set-union (inst-need seq1)
                            (inst-need seq2))
                 (set-union (set-diff (inst-mod seq1) regs)
                            (inst-mod seq2))
                 (append (mapcar (lambda (x)
                                   `(save (reg ,x))) regs)
                         (inst-inst seq1)
                         (mapcar (lambda (x)
                                   `(restore (reg ,x))) (reverse regs))
                         (inst-inst seq2)))))

(defun preserving (regs seq1 seq2)
  (if (null regs)
      (append-instruction seq1 seq2)
      (let ((reg (car regs)))
        (if (and (memq reg (inst-mod seq1))
                 (memq reg (inst-need seq2)))
            (preserving (cdr regs)
                        (make-inst
                         (set-union (list reg) (inst-need seq1))
                         (set-diff (inst-mod seq1) (list reg))
                         (append `((save ,reg))
                                 (inst-inst seq1)
                                 `((restore ,reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

;;; tocheck
(defun append-instruction (&rest args)
  (labels ((append-2-inst (seq1 seq2)
             (let ((n (set-union (inst-need seq1)
                                 (set-diff (inst-need seq2) (inst-mod seq1))))
                   (m (set-union (inst-mod seq1)
                                 (inst-mod seq2)))
                   (inst (append (inst-inst seq1)
                                 (inst-inst seq2))))
               (make-inst n m inst))))
    (reduce #'append-2-inst args)))

(defun tack-on-instruction (seq1 body-seq)
          (type-of seq1) seq1
          (type-of body-seq) body-seq)
  (make-inst (inst-need seq1)
             (inst-mod seq1)
             (append (inst-inst seq1) (inst-inst body-seq))))

(defun parallel-instruction (seq1 seq2)
  (make-inst (set-union (inst-need seq1) (inst-need seq2))
             (set-union (inst-mod seq1) (inst-mod seq2))
             (append (inst-inst seq1) (inst-inst seq2))))


(defun end-with-linkage (linkage seq)
  (preserving '(continue)
              seq
              (compile-linkage linkage)))

(defun $compile (exp target linkage)
  (cond
    ((atom exp)
     (cond
       ((self-eval? exp)
        (comp-self-eval exp target linkage))
       ((var-ref? exp)
        (comp-var-ref exp target linkage))
       (t
        (error "oops"))))
    (t
     (case (car exp)
       (quote
        (comp-quote-exp exp target linkage))
       (set!
        (comp-assign exp target linkage))
       (define
        (comp-define exp target linkage))
       (if
        (comp-if exp target linkage))
       (begin
        (comp-begin exp target linkage))
       (fn
        (comp-fn exp target linkage))
       (t
        (comp-appl exp target linkage))))
    ))

(defun self-eval? (exp)
  (or (typep exp 'boolean) (numberp exp) (stringp exp)))

(defun quoted-exp? (exp)
  (and (consp exp) (eq (car exp) 'quote)))

(defun var-ref? (exp)
  (symbolp exp))

;;; const, var, quote, if, fn, setq, application, ...
(defun comp-self-eval (exp target linkage)
  (end-with-linkage linkage
                    (make-inst '() (list target)
                               `((assign ,target
                                         (const ,exp))))))

(defun comp-quote-exp (exp target linkage)
  (end-with-linkage linkage
                    (make-inst '() (list target)
                               `((assign ,target
                                         (const ,(cadr exp)))))))

(defun comp-var-ref (exp target linkage)
  (end-with-linkage linkage
                    (make-inst '(env) (list target)
                               `((assign ,target
                                         (op lookup)
                                         (const ,exp)
                                         (reg env))))))

(defun comp-assign (exp target linkage)
  (let ((v-code ($compile (caddr exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 v-code
                 (make-inst '(env val) (list target)
                            `((perform (op set-var-value!)
                                       (const ,(cadr exp))
                                       (reg val)
                                       (reg env))
                              (assign ,target (const OK))))))))

(defun comp-define (exp target linkage)
  (let ((v-code ($compile (caddr exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 v-code
                 (make-inst '(env val) (list target)
                            `((perform (op define-var!)
                                       (const ,(cadr exp))
                                       (reg val)
                                       (reg env))
                              (assign ,target (const OK))))))))

(defvar *label-count* 1)
(defun make-label (&optional (lab "label"))
  (prog1
      (concatenate 'string lab (format nil "-~d" *label-count*))
    (setq *label-count* (1+ *label-count*))))

(defun comp-if (exp target linkage)
  (let* ((lab-after-if (make-label "FI"))
         (lab-then (make-label "LAB"))
         (then-link (if (eq linkage 'next)
                        lab-after-if
                        linkage))
         (testc ($compile (cadr exp) 'val 'next))
         (thenc ($compile (caddr exp) target linkage))
         (altc ($compile (cadddr exp) target then-link))
         (nextc (parallel-instruction
                 (append-instruction
                  altc
                 (make-inst nil nil
                            (list lab-then)))
                 (append-instruction
                  thenc
                  (make-inst nil nil
                             (list lab-after-if))))))
    (preserving '(env continue)         ;why?
                testc
                (append-instruction
                 (make-inst '(val) nil
                            `((test (op true?) (reg val))
                              (branch (label ,lab-then))))
                 nextc))))

(defun comp-begin (exp target linkage)
  (if (null (cdr exp))
      nil                               ;???
      (ccl::iterate lp ((seq (cdr exp)))
        (if (null (cdr seq))
            ($compile (car seq) target linkage)
            (preserving '(env continue) ;why preserve these regs?
                        ($compile (car seq) target 'next)
                        (lp (cdr seq)))))))

(defun comp-fn (exp target linkage)
  (let* ((lab-entry (make-label "ENTRY"))
         (lab-after-fn (make-label "END-ENTRY"))
         (lab-link (if (eq linkage 'next)
                       lab-after-fn
                       linkage)))
    (tack-on-instruction
     (end-with-linkage lab-link
                       (make-inst nil (list target)
                                  `((assign ,target
                                            (op make-fn)
                                            (label ,lab-entry)
                                            (reg env)))))
     (append-instruction
      (make-inst nil nil
                 (list lab-entry))
      (make-inst '(proc argl) '(env)
                 `((assign env (op fn-env) (reg proc))
                   (assign env
                           (op extend)
                           (const ,(cadr exp))
                           (reg argl)
                           (reg env))))
      (comp-begin (cddr exp) 'val 'return)
      (make-inst nil nil
                 `(,lab-after-fn))))))

(defun comp-appl (exp target linkage)
  (let ((fcode ($compile (car exp) 'proc 'next))
        (ocode (comp-operands (reverse (cdr exp)) 'argl 'next))
        (app-code ($apply target linkage)))
    ;; todo
    (preserving '(env continue)
                fcode
                (preserving '(proc continue)
                            ocode
                            ($apply target linkage)))))

(defun comp-operands (exp target linkage)
  (cond ((null exp) (make-inst nil '(argl) `((assign argl (const nil))))) ;ok, not (assign argl (op list) (const nil))
        (t
         (preserving '(env)
                     ($compile (car exp) 'val 'next)
                     (comp-operands-aux (cdr exp))))))

(defun comp-operands-aux (exp)
  (cond ((null exp)
         (make-empty-instruction))
        ((null (cdr exp))
         (append-instruction
          ($compile (car exp) 'val 'next)
          (make-inst nil '(argl)
                     `((assign argl (op cons) (reg val) (reg argl))))))
        (t
         (preserving '(env)             ;i don't know why
                     ($compile (car exp) 'val 'next)
                     (preserving '(argl)
                                 (make-inst '(val argl) '(argl)
                                            `((assign argl (op cons) (reg val) (reg argl))))
                                 (comp-operands-aux (cdr exp)))))))

(defun $apply (target linkage)
  (let* ((lab-prim (make-label "PRIM"))
         (lab-after-call (make-label "AFTER-CALL"))
         (link (if (eq linkage 'next)
                   lab-after-call
                   linkage)))
    (append-instruction
     (make-inst '(proc) '()
                `((test (op prim?) (reg proc))
                  (branch (label ,lab-prim))))
     (parallel-instruction
      (append-instruction
       (appl-code target link)
       (make-inst nil nil `(,lab-prim)))
      (append-instruction
       (end-with-linkage linkage
                         (make-inst '(proc argl) nil
                                    `((perform (op prim-apply)
                                               (reg proc)
                                               (reg argl)))))
       (make-inst nil nil `(,lab-after-call)))))))
                 

(defconstant all-regs '(proc argl env continue val))

(defun appl-code (target linkage)
  (let ((lab-ret (make-label "RET")))
    (cond ((eq target 'val)
           (cond ((eq linkage 'return)
                  (make-inst '(proc) all-regs
                             `((assign val (op fn-entry proc))
                               (goto (reg val)))))
                 (t                     ;linkage is label
                  (make-inst '(proc) all-regs
                             `((assign continue (label ,linkage))
                               (assign val (op fn-entry proc))
                               (goto (reg val)))))))
          (t
           (cond ((eq linkage 'return)
                  (error "return target not val"))
                 (t
                  (make-inst '(proc) all-regs
                             `((assign continue (label ,lab-ret))
                               (assign val (op fn-entry proc))
                               (goto (reg val))
                               ,lab-ret
                               (assign ,target (reg val))))))))))
                  


(defvar *nsucc* 0)
(defvar *nfail* 0)

(defmacro run-case
    (exp)
  (let ((ans (gensym)))
    `(progn
       (format t "test ~a " ',exp)
       (let ((,ans ,exp))
         (format t "=> ~%~a~%" ,ans)))))


(defun run-test ()
  ;; test linkage
  (run-case ($compile '5 'val 'return))
  (run-case ($compile '5 'val 'label))
  (run-case ($compile '5 'val 'next))

  ;; test atom
  (run-case ($compile "hello" 'val 'next))
  (run-case ($compile t 'val 'next))
  (run-case ($compile nil 'val 'next))

  ;; test set! & define
  (run-case ($compile '(set! a "inferno") 'val 'next))
  (run-case ($compile '(define max-depth 5) 'val 'next))

  ;; test begin
  (run-case ($compile '(begin) 'val 'next))
  (run-case ($compile '(begin 1) 'val 'next))
  (run-case ($compile '(begin 1 2) 'val 'next))
  (run-case ($compile '(begin 1 2 3) 'val 'next))

  ;; test if
  (run-case ($compile '(if t "hello" "world") 'val 'next))
  (run-case ($compile '(if t "hello" "world") 'val 'user-label))
  (run-case ($compile '(if t "hello" "world") 'val 'return))
  (run-case ($compile '(if t (begin 1 2 3) "world") 'val 'next))

  ;; test fn
  (run-case ($compile '(fn (x y) (+ x y)) 'proc 'next))
  (run-case ($compile '(fn (x y) nil) 'proc 'foo))

  ;; application
  (run-case ($compile '(if (= n 1)
                        1
                        (* (fact (- n 1) n)))
                      'val 'next))
  (run-case ($compile '(define fact
                        (fn (n)
                         (if (= n 1)
                             1
                             (* (fact (- n 1)) n))))
                      'val 'next))
  )
