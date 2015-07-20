;; Pseudo macro-expander for convenience

(load "qq.scm")

(define (macro-expand expr macros)
  (if (pair? expr)
      (map (lambda (e) (macro-expand e macros))
             ((macro-expander (car expr) macros) expr))
      expr))

(define (macro-expander name macros)
  (let ((macro (assoc name macros)))
    (if macro
        (cdr macro)
        id)))

(define (make-builtin-macros)
  (list (cons 'quasiquote quasiquote-macro)
        (cons 'define define-macro_)
        (cons 'when when-macro)
        (cons 'unless unless-macro)
        (cons 'cond cond-macro)
        (cons 'and and-macro)
        (cons 'or or-macro)
        (cons 'let let-macro)
        (cons 'let* let*-macro)))

(define (quasiquote-macro expr)
  (quasiquote-expand (cadr expr)))

;; FIXME There's already a 'define-macro' macro in Scheme...
(define (define-macro_ expr)
  (if (value-define? expr)
      expr
      `(define ,(define-name expr)
         (lambda (,@(define-args expr))
           ,(define-body expr)))))

(define (when-macro expr)
  `(if ,(conditional-predicate expr)
       (do ,@(conditional-body expr))
       nil))

(define (conditional-predicate expr)
  (cadr expr))

(define (conditional-body expr)
  (cddr expr))

(define (unless-macro expr)
  `(if ,(conditional-predicate expr)
       nil
       (do ,@(conditional-body expr))))

(define (cond-macro expr)
  (build-cond (cond-clauses expr)))

(define (build-cond clauses)
  (if (empty? clauses)
      'nil
      `(if ,(clause-predicate (first-clause clauses))
           (do ,@(clause-body (first-clause clauses)))
           ,(build-cond (rest-clauses clauses)))))

(define (cond-clauses expr)
  (cdr expr))

(define (clause-predicate clause)
  (car clause))

(define (clause-body clause)
  (cdr clause))

(define (and-macro expr)
  (build-and (short-circuit-clauses expr)))

(define (build-and clauses)
  (if (empty? (rest-clauses clauses))
      (first-clause clauses)
      `(if ,(first-clause clauses)
           ,(build-and (rest-clauses clauses))
           false)))

(define (short-circuit-clauses expr)
  (cdr expr))

(define (first-clause clauses)
  (car clauses))

(define (rest-clauses clauses)
  (cdr clauses))

(define (or-macro expr)
  (build-or (short-circuit-clauses expr)))

(define (build-or clauses)
  (if (empty? (rest-clauses clauses))
      (first-clause clauses)
      `(if ,(first-clause clauses)
           true
           ,(build-or (rest-clauses clauses)))))

(define (let-macro expr)
  (let ((args (map car (let-bindings expr)))
        (vals (map cadr (let-bindings expr)))
        (body (let-body expr)))
    `((lambda (,@args) ,@body)
      ,@vals)))

(define (let*-macro expr)
  (let ((args (map car (let-bindings expr)))
        (vals (map cadr (let-bindings expr)))
        (body (let-body expr)))
    (car (build-let* args vals body))))

(define (build-let* args vals body)
  (if (empty? args)
      body
      `(((lambda (,(car args))
           ,@(build-let* (cdr args)
                         (cdr vals)
                         body))
         ,(car vals)))))
