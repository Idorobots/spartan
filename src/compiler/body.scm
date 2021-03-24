;; Implicit body handling.

(load "compiler/utils/utils.scm")
(load "compiler/utils/errors.scm")

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
           (> (length exprs) 0))
      ;; NOTE The body spans all the expressions within it.
      (at (location (get-location-start (car exprs))
                    (get-location-end (last exprs)))
          (generated
           (context ctx
                    (make-do-node exprs))))
      (compiler-bug)))

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
    (cond ((> (length defs) 0)
           (replace expr
                    (generated
                     (make-letrec-node defs
                                       (reconstruct-simple-body non-defs expr)))))
          ((= (length exprs) 1)
           (car exprs))
          (else expr))))

(define (extract-defs exprs)
  (map (lambda (e)
         (cons (ast-def-name e)
               (ast-def-value e)))
       (filter (flip is-type? 'def)
               exprs)))

(define (extract-non-defs exprs)
  (filter (compose not (flip is-type? 'def))
          exprs))

(define (reconstruct-simple-body exprs parent)
  (let ((ctx (get-context* parent "Bad `do` syntax")))
    (cond ((= (length exprs) 0)
           (raise-compilation-error
            (get-location parent)
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
