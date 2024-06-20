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
     #:when (member op '(random current-task task-info))
     `(,op))

    ;; Monadic primops
    ((primop-app op a)
     #:when (member op '(not
                         zero?
                         car cdr nil? empty?
                         display
                         ref deref
                         uproc-pid uproc-state
                         uproc-delimited-continuations
                         uproc-error-handler
                         uproc-delimited-continuations
                         uproc-msg-queue-empty? uproc-dequeue-msg!
                         find-task wake-task!
                         assert! signal! retract! select))
     `(,op ,(generate-scheme-node a)))

    ;; Diadic primops
    ((primop-app op a b)
     #:when (member op '(eq? equal?
                         + - * / = < <= > >= modulo remainder quotient
                         cons append concat
                         set-uproc-delimited-continuations!
                         inc-uproc-rtime!
                         set-uproc-state!
                         uproc-enqueue-msg!
                         set-uproc-error-handler!
                         set-uproc-delimited-continuations!
                         notify-whenever
                         assign!))
     `(,op ,(generate-scheme-node a)
           ,(generate-scheme-node b)))

    ;; Diadic primops
    ((primop-app op a b)
     #:when (member op '(eq? equal?
                         + - * / = < <= > >= modulo remainder quotient
                         cons append concat
                         set-uproc-delimited-continuations!
                         inc-uproc-rtime!
                         set-uproc-state!
                         uproc-enqueue-msg!
                         notify-whenever
                         assign!))
     `(,op ,(generate-scheme-node a)
           ,(generate-scheme-node b)))

    ;; Triadic primops
    ((primop-app op a b c)
     #:when (member op '(spawn-task!))
     `(,op ,(generate-scheme-node a)
           ,(generate-scheme-node b)
           ,(generate-scheme-node c)))

    ;; Vararg primops
    ;; FIXME This should be removed as there shouldn't be any vararg primops.
    ((primop-app op args ...)
     #:when (member op '(list))
     `(,op ,@(map (lambda (a)
                    (generate-scheme-node a))
                  args)))

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
    ((primop-app '&yield-cont (symbol c) h)
     `(if (> (kont-counter) 0)
          (begin
            (dec-kont-counter!)
            ((closure-fun ,c)
             (closure-env ,c)
             ,(generate-scheme-node h)))
          (begin
            (reset-kont-counter!)
            (make-resumable ,c
                            ,(generate-scheme-node h)))))

    ((primop-app '&yield-cont k h)
     (let ((tmp (gensym 'tmp)))
       `(if (> (kont-counter) 0)
            (let ((,tmp ,(generate-scheme-node k)))
              (dec-kont-counter!)
              ((closure-fun ,tmp)
               (closure-env ,tmp)
               ,(generate-scheme-node h)))
            (begin
              (reset-kont-counter!)
              (make-resumable ,(generate-scheme-node k)
                              ,(generate-scheme-node h))))))

    ;; Exceptions
    ((primop-app '&error-handler)
     `(uproc-error-handler (current-task)))

    ((primop-app '&set-error-handler! handler)
     `(set-uproc-error-handler! (current-task) ,(generate-scheme-node handler)))

    ;; Modules & structures primops
    ((primop-app '&make-structure bindings ...)
     ;; FIXME Should be something better than an alist.
     `(list '&structure ,@(map (lambda (b)
                                 (generate-scheme-node b))
                               bindings)))

    ((primop-app '&structure-binding name value)
     `(cons ,(generate-scheme-node name)
            ,(generate-scheme-node value)))

    ((primop-app '&structure-ref s name)
     `(cdr (assoc ,(generate-scheme-node name)
                  (cdr ,(generate-scheme-node s)))))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))
