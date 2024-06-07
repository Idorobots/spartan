#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/utils.rkt")
(require "../../utils/gensym.rkt")

(provide generate-scheme)

;; TODOs
;; - [x] Implement proper code-gen.
;; - [ ] Remove no-longer-used primops from the runtime code.
;; - [ ] Update the test suite not to require interning the runtime all the time.
;; - [ ] Move the runtime code to a separate directory structure.
;; - [ ] Abstract common parts of the runtime to share between JS and Scheme.

(define (generate-scheme env)
  (let* ((init (env-get env 'init))
         (init-loc (ast-node-location init))
         (defs (env-get env 'data)))
    (if (empty? defs)
        (generate-scheme-node init)
        `(begin ,@(map (lambda (v)
                         (generate-scheme-def (car v) (cdr v)))
                       defs)
                ;; FIXME For consistency with JS this would ideally compile a module init function:
                ;; ,(generate-scheme-def '__module_init
                ;;                       (make-ast-lambda init-loc '() init))
                ;; (__module_init)
                ,(generate-scheme-node init)))))

(define (generate-scheme-def name value)
  `(define ,name
     ,(generate-scheme-node value)))

(define (generate-scheme-node expr)
  (match-ast expr
    ;; AST nodes
    ((string value)
    value)

    ((list vals ...)
     (map generate-scheme-node vals))

    ((number v)
     v)

    ((symbol s)
     s)

    ((const v)
     `(quote ,(generate-scheme-node v)))

    ((lambda args body)
     `(lambda ,(map generate-scheme-node args)
        ,(generate-scheme-node body)))

    ((if c t e)
     `(if ,(generate-scheme-node c)
          ,(generate-scheme-node t)
          ,(generate-scheme-node e)))

    ((let bindings body)
     `(let (,@(map (lambda (b)
                     (list (generate-scheme-node (ast-binding-var b))
                           (generate-scheme-node (ast-binding-val b))))
                   bindings))
        ,(generate-scheme-node body)))

    ((do exprs ...)
     `(begin ,@(map generate-scheme-node exprs)))

    ;; Nullary primops
    ((primop-app op)
     #:when (member op '(random newline self))
     `(,op))

    ;; Monadic primops
    ((primop-app op a)
     #:when (member op '(not
                         zero?
                         car cdr nil? empty?
                         display
                         ref deref
                         spawn
                         assert! signal! retract! select))
     `(,op ,(generate-scheme-node a)))

    ;; Diadic primops
    ((primop-app op a b)
     #:when (member op '(eq? equal?
                         + - * / = < <= > >= modulo remainder quotient
                         cons append concat
                         send notify-whenever
                         assign!))
     `(,op ,(generate-scheme-node a)
           ,(generate-scheme-node b)))

    ;; Vararg primops
    ((primop-app op args ...)
     #:when (member op '(list debug))
     `(,op ,@(map generate-scheme-node args)))

    ;; Closure primops
    ((primop-app '&make-env args ...)
     `(vector ,@(map generate-scheme-node args)))

    ((primop-app '&env-ref env offset)
     `(vector-ref ,(generate-scheme-node env)
                  ,(generate-scheme-node offset)))

    ((primop-app '&set-env! env offset value)
     `(vector-set! ,(generate-scheme-node env)
                   ,(generate-scheme-node offset)
                   ,(generate-scheme-node value)))

    ((primop-app '&make-closure env fun)
     `(vector &make-closure ;; FIXME Some internals require this to be the runtime function.
              ,(generate-scheme-node env)
              ,(generate-scheme-node fun)))

    ((primop-app '&set-closure-env! c env)
     `(vector-set! ,(generate-scheme-node c)
                   1
                   ,(generate-scheme-node env)))

    ((primop-app '&apply c args ...)
     (let ((tmp (gensym 'tmp)))
       `(let ((,tmp ,(generate-scheme-node c)))
          ((vector-ref ,tmp 2)
           (vector-ref ,tmp 1)
           ,@(map generate-scheme-node args)))))

    ;; Continuation primops
    ((primop-app '&yield-cont k h)
     (let ((tmp (gensym 'tmp)))
       `(if (> (kont-counter) 0)
            (let ((,tmp ,(generate-scheme-node k)))
              (dec-kont-counter!)
              ((vector-ref ,tmp 2)
               (vector-ref ,tmp 1)
               ,(generate-scheme-node h)))
            (begin
              (reset-kont-counter!)
              (resumable ,(generate-scheme-node k)
                         ,(generate-scheme-node h))))))

    ((primop-app '&push-delimited-continuation! k)
     (let ((tmp (gensym 'tmp))
           (uproc (gensym 'uproc)))
       `(let ((,tmp ,(generate-scheme-node k))
              (,uproc (current-task)))
          (set-uproc-delimited-continuations! ,uproc
                                              (cons ,tmp
                                                    (uproc-delimited-continuations ,uproc))))))

    ((primop-app '&pop-delimited-continuation!)
     (let ((stack (gensym 'stack))
           (uproc (gensym 'uproc)))
       `(let ((,uproc (current-task)))
          (let ((,stack (uproc-delimited-continuations ,uproc)))
            (set-uproc-delimited-continuations! ,uproc
                                                (cdr ,stack))
            (car ,stack)))))

    ;; Exceptions
    ((primop-app '&error-handler)
     `(uproc-error-handler (current-task)))

    ((primop-app '&set-error-handler! handler)
     `(set-uproc-error-handler! (current-task) ,(generate-scheme-node handler)))

    ;; Modules & structures primops
    ((primop-app '&make-structure bindings ...)
     ;; FIXME Should be something better than an alist.
     `(list &make-structure ,@(map generate-scheme-node bindings)))

    ((primop-app '&structure-binding name value)
     `(cons ,(generate-scheme-node name)
            ,(generate-scheme-node value)))

    ((primop-app '&structure-ref s name)
     `(cdr (assoc ,(generate-scheme-node name)
                  (cdr ,(generate-scheme-node s)))))

    ;; Currently not-primops primops:
    ;; call/current-continuation
    ;; call/reset
    ;; call/shift
    ;; call/handler
    ;; raise
    ;; recv
    ;; sleep

    ((primop-app op args ...)
     ;; FIXME These are resulting from the current instrumentation setup, but should never happen otherwise.
     `(,op ,@(map generate-scheme-node args)))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))
