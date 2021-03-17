;; Pseudo macro-expander for convenience

(load "compiler/ast.scm")
(load "compiler/utils.scm")

;; FIXME Implement in terms of AST walk.
(define (macro-expand expr macros)
  (if (and (pair? expr) (not (quote? expr)))
      (let ((r ((macro-expander (car expr) macros) expr)))
        (if (pair? r)
            (map (flip macro-expand macros) r)
            (macro-expand r macros)))
      expr))

(define (macro-expander name macros)
  (let ((macro (assoc name macros)))
    (if macro
        (cdr macro)
        id)))

(define (make-builtin-macros)
  (list (cons 'when when-macro)
        (cons 'unless unless-macro)
        (cons 'cond cond-macro)
        (cons 'and and-macro)
        (cons 'or or-macro)
        (cons 'let let-macro)
        (cons 'let* let*-macro)
        (cons 'letcc letcc-macro)
        (cons 'handle handle-macro)
        (cons 'shift shift-macro)
        (cons 'reset reset-macro)
        ;; FIXME These should be moved to semantic elaboration phase.
        (cons 'structure structure-macro)
        (cons 'module module-macro)))

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
    `((lambda (,@args) ,body)
      ,@vals)))

(define (let*-macro expr)
  (foldr (lambda (b acc)
           `((lambda (,(car b))
               ,acc)
             ,(cadr b)))
         (let-body expr)
         (let-bindings expr)))

(define (letcc-macro expr)
  `(call/current-continuation
    (lambda (,(letcc-cont expr))
      (do ,@(letcc-body expr)))))

(define (letcc-cont expr)
  (cadr expr))

(define (letcc-body expr)
  (cddr expr))

(define (shift-macro expr)
  `(call/shift
    (lambda (,(shift-cont expr))
      ,(shift-expr expr))))

(define (shift-cont expr)
  (cadr expr))

(define (shift-expr expr)
  (caddr expr))

(define (reset-macro expr)
  `(call/reset
    (lambda ()
      ,(reset-expr expr))))

(define (reset-expr expr)
  (cadr expr))

(define (handle-macro expr)
  `(call/handler
    ,(handle-handler expr)
    (lambda ()
      ,(handle-expr expr))))

(define (handle-handler expr)
  (caddr expr))

(define (handle-expr expr)
  (cadr expr))

(define (structure-macro expr)
  (let* ((lambdas (map (lambda (def)
                         `(,(define-name def)
                           ,(define-value def)))
                       (structure-defs expr)))
         (names (map car lambdas)))
    `(letrec (,@lambdas)
       (&make-structure ,@(map (lambda (n)
                                 `(&structure-binding ',n ,n))
                               names)))))

(define (module-macro expr)
  `(define ,(module-name expr)
     (lambda ,(module-deps expr)
       (structure ,@(module-body expr)))))
