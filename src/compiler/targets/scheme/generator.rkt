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
        (generate-scheme-node 'invalid-continuation init)
        `(begin ,@(map (lambda (v)
                         (generate-scheme-def (car v) (cdr v)))
                       defs)
                ;; FIXME For consistency with JS this would ideally compile a module init function:
                ;; ,(generate-scheme-def '__module_init
                ;;                       (make-ast-lambda init-loc '() init))
                ;; (__module_init)
                ,(generate-scheme-node 'invalid-continuation init)))))

(define (generate-scheme-def name value)
  `(define ,name
     ,(generate-scheme-node 'invalid-continuation value)))

(define (generate-scheme-node curr-cont expr)
  (match-ast expr
    ;; AST nodes
    ((string value)
    value)

    ((list vals ...)
     (map (lambda (v)
            (generate-scheme-node curr-cont v))
          vals))

    ((number v)
     v)

    ((symbol s)
     s)

    ((const v)
     `(quote ,(generate-scheme-node curr-cont v)))

    ((lambda args body)
     `(lambda ,(map (lambda (a)
                      (generate-scheme-node curr-cont a))
                    args)
        ,(generate-scheme-node (ast-symbol-value (last args)) body)))

    ((if c t e)
     `(if ,(generate-scheme-node curr-cont c)
          ,(generate-scheme-node curr-cont t)
          ,(generate-scheme-node curr-cont e)))

    ((let bindings body)
     `(let (,@(map (lambda (b)
                     (list (generate-scheme-node curr-cont (ast-binding-var b))
                           (generate-scheme-node curr-cont (ast-binding-val b))))
                   bindings))
        ,(generate-scheme-node curr-cont body)))

    ((do exprs ...)
     `(begin ,@(map (lambda (e)
                      (generate-scheme-node curr-cont e))
                    exprs)))

    ;; Nullary primop
    ((primop-app op)
     #:when (member op '(newline random self recv))
     `(,op))

    ;; Monadic primops
    ((primop-app op a)
     #:when (member op '(not
                         zero?
                         car cdr nil? empty?
                         display
                         ref deref
                         spawn sleep
                         assert! signal! retract! select))
     `(,op ,(generate-scheme-node curr-cont a)))

    ;; Diadic primops
    ((primop-app op a b)
     #:when (member op '(eq? equal?
                         + - * / = < <= > >= modulo remainder quotient
                         cons append concat
                         send notify-whenever
                         assign!))
     `(,op ,(generate-scheme-node curr-cont a)
           ,(generate-scheme-node curr-cont b)))

    ;; Vararg primops
    ((primop-app op args ...)
     #:when (member op '(list debug))
     `(,op ,@(map (lambda (a)
                    (generate-scheme-node curr-cont a))
                  args)))

    ;; RT system primops
    ((primop-app op args ...)
     #:when (member op '(current-task task-info
                         uproc-error-handler set-uproc-error-handler!
                         uproc-delimited-continuations set-uproc-delimited-continuations!
                         ;; FIXME Remove once the core is refactored.
                         pop-delimited-continuation! push-delimited-continuation!))
     `(,op ,@(map (lambda (a)
                    (generate-scheme-node curr-cont a))
                  args)))

    ;; Closure primops
    ((primop-app '&make-env args ...)
     `(vector ,@(map (lambda (a)
                       (generate-scheme-node curr-cont a))
                     args)))

    ((primop-app '&env-ref env offset)
     `(vector-ref ,(generate-scheme-node curr-cont env)
                  ,(generate-scheme-node curr-cont offset)))

    ((primop-app '&set-env! env offset value)
     `(vector-set! ,(generate-scheme-node curr-cont env)
                   ,(generate-scheme-node curr-cont offset)
                   ,(generate-scheme-node curr-cont value)))

    ((primop-app '&make-closure env fun)
     `(make-closure ,(generate-scheme-node curr-cont env)
                    ,(generate-scheme-node curr-cont fun)))

    ((primop-app '&set-closure-env! c env)
     `(set-closure-env! ,(generate-scheme-node curr-cont c)
                        ,(generate-scheme-node curr-cont env)))

    ((primop-app '&apply (symbol c) args ...)
     `((closure-fun ,c)
       (closure-env ,c)
       ,@(map (lambda (a)
                (generate-scheme-node curr-cont a))
              args)))

    ((primop-app '&apply c args ...)
     (let ((tmp (gensym 'tmp)))
       `(let ((,tmp ,(generate-scheme-node curr-cont c)))
          ((closure-fun ,tmp)
           (closure-env ,tmp)
           ,@(map (lambda (a)
                    (generate-scheme-node curr-cont a))
                  args)))))

    ;; Continuation primops
    ((primop-app '&current-continuation)
     (if (equal? curr-cont 'invalid-continuation)
         ;; FIXME This ideally would be checked in the validation.
         (compiler-bug "Invalid `&current-continuation` application:" expr)
         curr-cont))

    ((primop-app '&yield-cont (symbol c) h)
     `(if (> (kont-counter) 0)
          (begin
            (dec-kont-counter!)
            ((closure-fun ,c)
             (closure-env ,c)
             ,(generate-scheme-node curr-cont h)))
          (begin
            (reset-kont-counter!)
            (make-resumable ,c
                            ,(generate-scheme-node curr-cont h)))))

    ((primop-app '&yield-cont k h)
     (let ((tmp (gensym 'tmp)))
       `(if (> (kont-counter) 0)
            (let ((,tmp ,(generate-scheme-node curr-cont k)))
              (dec-kont-counter!)
              ((closure-fun ,tmp)
               (closure-env ,tmp)
               ,(generate-scheme-node curr-cont h)))
            (begin
              (reset-kont-counter!)
              (make-resumable ,(generate-scheme-node curr-cont k)
                              ,(generate-scheme-node curr-cont h))))))

    ((primop-app '&push-delimited-continuation! k)
     (let ((tmp (gensym 'tmp))
           (uproc (gensym 'uproc)))
       `(let ((,tmp ,(generate-scheme-node curr-cont k))
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
     `(set-uproc-error-handler! (current-task) ,(generate-scheme-node curr-cont handler)))

    ;; Modules & structures primops
    ((primop-app '&make-structure bindings ...)
     ;; FIXME Should be something better than an alist.
     `(list '&structure ,@(map (lambda (b)
                                 (generate-scheme-node curr-cont b))
                               bindings)))

    ((primop-app '&structure-binding name value)
     `(cons ,(generate-scheme-node curr-cont name)
            ,(generate-scheme-node curr-cont value)))

    ((primop-app '&structure-ref s name)
     `(cdr (assoc ,(generate-scheme-node curr-cont name)
                  (cdr ,(generate-scheme-node curr-cont s)))))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))
