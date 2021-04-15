;; Implicit body handling.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")
(load "compiler/errors.scm")

(define body-expand
  (pass (schema 'errors a-list?
                'ast (ast-subset? '(quote quasiquote unquote unquote-splicing
                                    number symbol string list
                                    if do let letrec binding lambda app def
                                    primop-app body <error> <location>)))
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (expand-body (env-get env 'ast))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define (expand-body expr)
  (map-ast id
           (lambda (expr)
             (ast-case expr
              ((body . ,exprs)
               (let* ((defs (extract-defs exprs))
                      (non-defs (extract-non-defs exprs)))
                 (cond ((> (length defs) 0)
                        (replace expr
                                 (generated
                                  (make-letrec-node defs
                                                    (reconstruct-simple-body non-defs expr)))))
                       ((= (length exprs) 1)
                        (car exprs))
                       (else (reconstruct-simple-body exprs expr)))))
              (else expr)))
           expr))

(define (extract-defs exprs)
  (foldr (lambda (e acc)
           (ast-case e
             ((def ,name ,value) (cons (replace e (generated (make-binding-node name value)))
                                       acc))
            (else acc)))
         '()
         exprs))

(define (extract-non-defs exprs)
  (filter (compose not def-node?)
          exprs))

(define (reconstruct-simple-body exprs parent)
  (let ((ctx (get-context* parent "Bad `do` syntax")))
    (cond ((= (length exprs) 0)
           (raise-compilation-error
            parent
            (format "~a, expected at least one non-definition expression within:" ctx)))
          ((= (length exprs) 1)
           (car exprs))
          (else
           ;; NOTE Reconstructed body location now has to be adjusted.
           (at (location (get-location-start (car exprs))
                         (get-location-end (last exprs)))
               (generated
                ;; However the context should be preserved.
                (context ctx
                         (make-do-node exprs))))))))
