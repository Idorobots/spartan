;; Implicit body handling.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/errors.scm")
(load "compiler/tree-ast.scm")

(define (body-expand env)
  (let ((result (collect-errors (env-get env 'errors)
                                (lambda ()
                                  (expand-body (env-get env 'ast))))))
    (env-set env
             'ast (car result)
             'errors (cadr result))))

;; FIXME This is used by several different phases to store context for body expansion later on
;; FIXME to get more meaningful error messages. This should probably be done differently.
(define (wrap-with-do exprs ctx)
  (if (and (list? exprs)
           (> (length exprs) 1))
      ;; NOTE The body spans all the expressions within it.
      (at (location (get-location-start (car exprs))
                    (get-location-end (last exprs)))
          (generated
           (context ctx
                    (make-do-node exprs))))
      (car exprs)))

(define (expand-body expr)
  (map-ast (lambda (expr)
             (case (get-type expr)
               ((do) (reconstruct-body expr))
               (else expr)))
           id
           expr))

(define (reconstruct-body expr)
  (let* ((exprs (ast-do-exprs expr))
         (defs (extract-defs exprs))
         (non-defs (extract-non-defs exprs)))
    (cond ((= (length non-defs) 0)
           (raise-compilation-error
            (get-location expr)
            (format "~a, expected at least one non-definition expression within:" (extract-context expr))))
          ((> (length defs) 0)
           (replace expr
                    (generated
                     (make-letrec-node defs
                                       (wrap-with-do non-defs (extract-context expr))))))
          (else expr))))

(define (extract-context expr)
  (let ((ctx (get-context expr)))
    (if (empty? ctx)
        "Bad `do` syntax"
        ctx)))

(define (extract-defs exprs)
  (map (lambda (e)
         (cons (ast-def-name e)
               (ast-def-value e)))
       (filter (flip is-type? 'def)
               exprs)))

(define (extract-non-defs exprs)
  (filter (compose not (flip is-type? 'def))
          exprs))
