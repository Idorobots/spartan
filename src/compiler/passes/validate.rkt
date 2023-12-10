#lang racket

;; Final frontend code validation.

(require "../utils/set.rkt")
(require "../utils/utils.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")

(provide validate
         ;; FIXME For test access.
         validate-ast extract-node-type)

(define validate
  (pass (schema "validate"
                'errors a-list?
                'globals a-set?
                'ast (ast-subset? '(const symbol
                                    if do let letrec binding lambda app
                                    primop-app def <error>)))
   (lambda (env)
     (let ((result (collect-errors (env-get env 'errors)
                                   (lambda ()
                                     (let ((expr (env-get env 'ast))
                                           (globals (env-get env 'globals)))
                                       (validate-ast globals
                                                     (get-undefined-vars expr globals)
                                                     (set)
                                                     (set)
                                                     expr))))))
       (env-set env
                'ast (car result)
                'errors (cadr result))))))

(define (get-undefined-vars expr globals)
  (set-difference (ast-node-free-vars expr) globals))

(define (validate-ast known undefined unused used-before-def expr)
  (match-ast expr
    ((const _)
     ;; TODO Validate number ranges, string escape sequences, unicode well-formedness etc.
     expr)
    ((lambda formals body)
     (let* ((bound (ast-node-bound-vars expr))
            (known (set-union known bound))
            (body-fv (ast-node-free-vars body))
            (unused (set-difference bound body-fv))
            (before-def (set-difference used-before-def body-fv)))
       (-> expr
           (set-ast-lambda-formals (map (partial validate-ast known (set) unused before-def) formals))
           (set-ast-lambda-body (validate-ast known
                                              (set-difference undefined bound)
                                              (set)
                                              before-def
                                              body)))))
    ((let bindings body)
     (let* ((bound (ast-node-bound-vars expr))
            (known (set-union known bound))
            (unused (set-difference bound (ast-node-free-vars body))))
       (-> expr
           (set-ast-let-bindings (map (partial validate-ast known undefined unused used-before-def) bindings))
           (set-ast-let-body (validate-ast known
                                           (set-difference undefined bound)
                                           (set)
                                           used-before-def
                                           body)))))
    ((letrec bindings body)
     (let* ((bound (ast-node-bound-vars expr))
            (known (set-union known bound))
            (without-bound (set-difference undefined bound))
            (unused (set-difference bound
                                    (set-union (ast-node-free-vars body)
                                               (set-sum (map ast-node-free-vars bindings))))))
       (-> expr
           (set-ast-letrec-bindings (validate-use-before-definition known
                                                                    bound
                                                                    without-bound
                                                                    unused
                                                                    used-before-def
                                                                    bindings))
           (set-ast-letrec-body (validate-ast known
                                              without-bound
                                              (set)
                                              used-before-def
                                              body)))))
    ((binding var val)
     (-> expr
         (set-ast-binding-var (validate-ast known
                                            (set)
                                            unused
                                            (set-difference used-before-def
                                                            (ast-node-bound-vars expr))
                                            var))
         (set-ast-binding-val (validate-ast known
                                            undefined
                                            (set)
                                            used-before-def
                                            val))))
    ((symbol '_)
    ;; NOTE Wildcards are always fine.
     expr)
    ((symbol value)
     (cond ((generated? expr)
            expr)
           ((set-member? undefined value)
            (let* ((value-str (symbol->string value))
                   (scored (sort (map (lambda (v)
                                        (cons (levenshtein-distance (symbol->string v)
                                                                    value-str)
                                              v))
                                      known)
                                 (lambda (a b)
                                   (< (car a)
                                      (car b))))))
              (raise-compilation-error
               expr
               (if (and (not (empty? scored))
                        ;; 5 is arbitrarily selected to make sure we only propose close-enough defined variables.
                        (<= (caar scored) 5)
                        ;; This ensures we won't go overboard with short variable names proposals.
                        (<= (caar scored) (string-length value-str)))
                   (format "Undefined variable `~a`, did you mean `~a`:" value (cdar scored))
                   (format "Undefined variable `~a`:" value)))))
           ((set-member? used-before-def value)
            (raise-compilation-error
             expr
             (format "Variable `~a` used before its definition:" value)))
           ((set-member? unused value)
            (raise-compilation-error
             expr
             (format "Unused variable `~a`, rename to `_` to avoid this error:" value)))
           (else
            expr)))
    ((app op args ...)
     (-> expr
         (set-ast-app-op (validate-app-procedure (validate-ast known undefined unused used-before-def op)))
         (set-ast-app-args (map (partial validate-ast known undefined unused used-before-def) args))))
    ((def name value)
     (let* ((bound (ast-node-bound-vars expr))
            (knwon (set-union known bound))
            (unused (set-difference bound (ast-node-free-vars value))))
       (raise-compilation-error
        ;; NOTE So that we might find more meaningful errors in there.
        (-> expr
           (set-ast-def-name (validate-ast known
                                           (set)
                                           unused
                                           used-before-def
                                           name))
           (set-ast-def-value (validate-ast known
                                            (set-difference undefined bound)
                                            (set)
                                            used-before-def
                                            value)))
        (format "~a, not allowed in this context:" (ast-node-context* expr "Bad `define` syntax")))))
    (else
     (walk-ast (partial validate-ast known undefined unused used-before-def)
               expr))))

(define (validate-use-before-definition known bound undefined unused used-before-def bindings)
  ;; NOTE Simple values are considered seen since they will be extracted outward.
  (let loop ((seen (set-sum (map ast-node-bound-vars
                                 (filter (compose (partial equal? 'simple) ast-binding-complexity)
                                         bindings))))
             (bs bindings))
    (if (empty? bs)
        '()
        (let ((b (car bs)))
          ;; NOTE Eager free vars are considered undefined unless defined in order of usage.
          (cons (validate-ast known
                              undefined
                              unused
                              (set-union used-before-def
                                         (set-difference (set-intersection (ast-node-free-vars b)
                                                                           bound)
                                                         seen))
                              b)
                (loop (set-union seen (ast-node-bound-vars b))
                      (cdr bs)))))))

(define (extract-node-type node)
  (match-ast node
   ((const (symbol _))
    ;; FIXME This would ideally report a "constant symbol"
    'quote)
   ((const expr)
    (ast-node-type expr))
   ((ast-error expr)
    (extract-node-type expr))
   (else
    (ast-node-type node))))

(define (validate-app-procedure op)
  (let ((type (extract-node-type op)))
    (if (member type '(symbol if do body lambda let letrec app primop-app))
        op
        (raise-compilation-error
         op
         (format "Bad call syntax, expected an expression that evaluates to a procedure but got a ~a instead:" type)))))
