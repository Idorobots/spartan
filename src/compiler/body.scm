;; Implicit body handling.

(load "compiler/utils.scm")
(load "compiler/env.scm")
(load "compiler/tree-ast.scm")

(define (body-expand env)
  (env-update env 'ast expand-body))

(define (expand-body expr)
  (map-ast id
           (lambda (expr)
             (case (get-type expr)
               ((lambda let letrec) (ast-update expr 'body wrap-sequential))
               (else expr)))
           expr))

(define (wrap-sequential exprs)
  (cond ((and (list? exprs)
              (> (length exprs) 1))
         ;; NOTE The body spans all the expressions within it.
         (at (location (get-location-start (car exprs))
                       (get-location-end (last exprs)))
             (generated
              (make-do-node exprs))))
        ((list? exprs)
         (car exprs))
        (else exprs)))
