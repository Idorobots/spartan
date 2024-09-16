#lang racket

;; Copy propagation.

(require "../utils/utils.rkt")

(require "../substitute.rkt")
(require "../propagate.rkt")
(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(provide propagate-copies
         ;; FIXME For test access.
         copy-propagation)

(define propagate-copies
  (pass (schema "propagate-copies"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))
        (lambda (env)
          (env-update env 'ast (partial copy-propagation (make-subs '()))))
        (schema "propagate-copies output"
                'ast (ast-subset? '(const symbol if do let letrec fix binding lambda app primop-app)))))

(define (copy-propagation subs expr)
  (propagate symbol-binding?
             make-copy-subs
             (lambda (subs expr kont)
               (if (ast-symbol? expr)
                   (replace-copy subs expr)
                   (kont expr)))
             subs
             expr))

(define (replace-copy subs expr)
  (let ((s (ast-symbol-value expr)))
    (replace-sub subs
                 s
                 (constantly expr))))

(define (symbol-binding? binding)
  (ast-symbol? (ast-binding-val binding)))

(define (make-copy-subs bindings subs)
  (extend-subs (map (lambda (binding)
                      (let ((val (ast-binding-val binding)))
                        (cons (ast-symbol-value (ast-binding-var binding))
                              ;; NOTE To avoid reintroducing potentially no-longer-defined variables...
                              ;; NOTE ...but also not overwrite copy substitutions with values bound later.
                              (if (ast-symbol? val)
                                  (replace-copy subs val)
                                  val))))
                    bindings)
               subs))
