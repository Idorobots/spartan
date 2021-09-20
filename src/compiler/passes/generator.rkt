#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast/utils.rkt")

(provide generate-target-code)

(define generate-target-code
  (pass (schema "generate-target-code"
                'data (list-of? (a-pair? a-symbol?
                                         (ast-subset? '(const symbol if do let binding lambda primop-app))))
                'init (ast-subset? '(const symbol if do let binding primop-app)))
        (lambda (env)
          ;; FIXME Actually implement a proper code-gen.
          (let ((data (map (lambda (v)
                             (list 'define
                                   (car v)
                                   (ast->plain (cdr v))))
                           (env-get env 'data)))
                (init (ast->plain (env-get env 'init))))
            (if (empty? data)
                init
                `(begin ,@data
                        ,init))))))
