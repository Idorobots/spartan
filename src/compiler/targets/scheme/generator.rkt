#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/utils.rkt")
(require "../../utils/gensym.rkt")

(provide generate-scheme)

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
     (map (lambda (v)
            (generate-scheme-node v))
          vals))

    ((number v)
     v)

    ((symbol s)
     s)

    ((const v)
     `(quote ,(generate-scheme-node v)))

    ((lambda formals body)
     `(lambda ,(map (lambda (a)
                      (generate-scheme-node a))
                    formals)
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
     `(begin ,@(map (lambda (e)
                      (generate-scheme-node e))
                    exprs)))

    ;; Nullary primop
    ((primop-app op)
     #:when (member op '(random current-milliseconds))
     `(,op))

    ;; Monadic primops
    ((primop-app op a)
     #:when (member op '(suspend resumable? resume trampoline
                         car cdr
                         display delay-milliseconds
                         ref deref
                         uproc-pid uproc-priority uproc-state uproc-vtime uproc-rtime
                         uproc-continuation uproc-delimited-continuations uproc-error-handler
                         uproc-msg-queue-empty? uproc-dequeue-msg!
                         assert! signal! retract! select))
     `(,op ,(generate-scheme-node a)))

    ;; Diadic primops
    ((primop-app op a b)
     #:when (member op '(eq? equal? cons
                         + - * / = < <= > >= modulo remainder quotient
                         set-uproc-rtime! set-uproc-state! uproc-enqueue-msg!
                         set-uproc-continuation! set-uproc-delimited-continuations! set-uproc-error-handler!
                         whenever-trampoline assign!))
     `(,op ,(generate-scheme-node a)
           ,(generate-scheme-node b)))

    ;; Other primops
    ((primop-app 'make-uproc prio cont handler time state)
     `(make-uproc ,(generate-scheme-node prio)
                  ,(generate-scheme-node cont)
                  ,(generate-scheme-node handler)
                  ,(generate-scheme-node time)
                  ,(generate-scheme-node state)))

    ;; Closure primops
    ((primop-app '&make-env args ...)
     `(vector ,@(map (lambda (a)
                       (generate-scheme-node a))
                     args)))

    ((primop-app '&env-ref env offset)
     `(vector-ref ,(generate-scheme-node env)
                  ,(generate-scheme-node offset)))

    ((primop-app '&set-env! env offset value)
     `(vector-set! ,(generate-scheme-node env)
                   ,(generate-scheme-node offset)
                   ,(generate-scheme-node value)))

    ((primop-app '&make-closure env fun)
     `(make-closure ,(generate-scheme-node env)
                    ,(generate-scheme-node fun)))

    ((primop-app '&set-closure-env! c env)
     `(set-closure-env! ,(generate-scheme-node c)
                        ,(generate-scheme-node env)))

    ((primop-app '&apply (symbol c) args ...)
     `((closure-fun ,c)
       (closure-env ,c)
       ,@(map (lambda (a)
                (generate-scheme-node a))
              args)))

    ((primop-app '&apply c args ...)
     (let ((tmp (gensym 'tmp)))
       `(let ((,tmp ,(generate-scheme-node c)))
          ((closure-fun ,tmp)
           (closure-env ,tmp)
           ,@(map (lambda (a)
                    (generate-scheme-node a))
                  args)))))

    ;; Continuation primops
    ((primop-app '&yield-cont-immediate (symbol c) h)
     `(begin
        (reset-kont-counter!)
        (make-resumable ,c
                        ,(generate-scheme-node h))))

    ((primop-app '&yield-cont-immediate k h)
     (let ((tmp (gensym 'tmp))
           (loc (ast-node-location k)))
       `(let ((,tmp ,(generate-scheme-node k)))
          ,(generate-scheme-node
            (set-ast-primop-app-args expr
                                     (list (make-ast-symbol loc tmp)
                                           h))))))

    ((primop-app '&yield-cont (symbol c) h)
     `(if (> (kont-counter) 0)
          (begin
            (dec-kont-counter!)
            ((closure-fun ,c)
             (closure-env ,c)
             ,(generate-scheme-node h)))
          ,(generate-scheme-node
            (set-ast-primop-app-op expr '&yield-cont-immediate))))

    ((primop-app '&yield-cont k h)
     (let ((tmp (gensym 'tmp))
           (loc (ast-node-location k)))
       `(let ((,tmp ,(generate-scheme-node k)))
          ,(generate-scheme-node
            (set-ast-primop-app-args expr
                                     (list (make-ast-symbol loc tmp)
                                           h))))))

    ;; Modules & structures primops
    ((primop-app '&make-structure bindings ...)
     ;; FIXME Should be something r7rs-compatible.
     `(make-hasheq
       (list ,@(map (lambda (b)
                      (generate-scheme-node b))
                    bindings))))

    ((primop-app '&structure-binding name value)
     `(cons ,(generate-scheme-node name)
            ,(generate-scheme-node value)))

    ((primop-app '&structure-ref s name)
     `(hash-ref
       ,(generate-scheme-node s)
       ,(generate-scheme-node name)))

    ((primop-app '&primitive-metadata n meta ...)
     '())

    (else
     (compiler-bug "Unsupported AST node type:" expr))))
