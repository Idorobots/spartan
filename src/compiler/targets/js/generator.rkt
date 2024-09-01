#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/assets.rkt")
(require "../../utils/utils.rkt")
(require "../../utils/gensym.rkt")
(require "../../passes/rename.rkt") ;; FIXME For symbol->safe

;; FIXME This currently results in a cycle dependency. Should be refactored with first-class targets.
(require racket/lazy-require)
(lazy-require ["../../core.rkt" (compile-core-spartan)]) ;; FIXME For the core module compilation.

(provide generate-js)

(define +js-continuation-hops+ 1) ;; FIXME Causes a stack overflow already at pretty low values.
(define +js-bootstrap+ (embed-file-contents "./bootstrap.js"))
(define +js-runtime+ (embed-file-contents "./rt.js"))

(define (generate-js e)
  ;; Generate JavaScript code for the root node
  (let* ((init (env-get e 'init))
         (init-loc (ast-node-location init))
         (defs (env-get e 'data))
         (unknown-loc (location 0 0))
         (js-defs (foldr (lambda (v acc)
                           (string-append
                            (generate-js-def (car v) (cdr v))
                            acc))
                         ""
                         defs))
         (module-name (safe-module-name e))
         (js-init (generate-js-def module-name
                                   (generated
                                    (make-ast-lambda init-loc '() init)))))
    (if (env-get* e 'omit-runtime #f)
        (string-append js-defs js-init)
        (string-append
         (generate-js-def '__rt_continuation_hops
                          (generated
                           (make-ast-const unknown-loc
                                           (make-ast-number unknown-loc
                                                            +js-continuation-hops+))))
         +js-bootstrap+
         ;; FIXME Needs to be done on each compilation to avoid dependency cycles.
         (let ((output (compile-core-spartan (env 'target 'ES6
                                                  'omit-runtime #t))))
           (env-get output 'generated))
         js-defs
         js-init
         ;; NOTE The runtime expects a specific module as the init one.
         (generate-js-def '__module_init
                          (generated
                           (make-ast-symbol unknown-loc module-name)))
         +js-runtime+))))

(define (safe-module-name env)
  (-> env
      (env-get 'module)
      (string-replace ".sprtn" "")
      string->symbol
      symbol->safe))

(define (generate-js-def name value)
  (format "const ~a = ~a;~n"
          name
          (generate-js-node value id)))

(define (generate-const-node expr)
  (match-ast expr
    ((string value)
     (format "~v" value))

    ((list vals ...)
     (foldr (lambda (v acc)
              (format "makeCons(~a, ~a)"
                      (generate-const-node v)
                      acc))
            "null"
            vals))

    ((number v)
     (format "~a" v))

    ((symbol s)
     (format "\"~a\"" s))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))

(define-syntax <-
  (syntax-rules ()
    ((<- ((var (call ...))) body ...)
     (call ... (lambda (var)
                 body ...)))
    ((<- ((var (call ...)) rest ...) body ...)
     (call ... (lambda (var)
                 (<- (rest ...)
                     body ...))))))

(define (generate-js-node expr return)
  (match-ast expr
    ;; AST nodes
    ((symbol s)
     (return
      (format "~a" s)))

    ((const v)
     (return
      (generate-const-node v)))

    ((lambda args body)
     (return
      (format "((~a) => {~a})"
              (string-join (map (lambda (a)
                                  (generate-js-node a id))
                                args)
                           ", ")
              (generate-js-node body
                                (lambda (v)
                                  (format "return ~a" v))))))

    ((if condition t e)
     ;; FIXME Booleans should be treated according to the language semantics.
     (<- ((c (generate-js-node condition)))
         (format "if (~a) { ~a } else { ~a }"
                 c
                 (generate-js-node t return)
                 (generate-js-node e return))))

    ((let bindings body)
     (foldr (lambda (b acc)
              (format "const ~a = ~a; ~a"
                      (generate-js-node (ast-binding-var b) id)
                      ;; FIXME This should ideally be handled with generate-js-sequence.
                      (generate-js-node (ast-binding-val b) id)
                      acc))
            (generate-js-node body return)
            bindings))

    ((do exprs ...)
     (<- ((es (generate-js-sequence exprs)))
         (let ((l (last es))
               (statements (take es (- (length es) 1))))
           (format "~a; ~a"
                   (string-join statements "; ")
                   (return l)))))

    ;; Mutable primops
    ((primop-app 'ref a)
     (<- ((v (generate-js-node a)))
         (return (format "makeRef(~a)" v))))

    ((primop-app 'deref a)
     (<- ((v (generate-js-node a)))
         (return (format "deref(~a)" v))))

    ((primop-app 'assign! a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "assign(~a, ~a)" a-js b-js))))

    ;; Math primops
    ((primop-app op a b)
     #:when (member op '(+ - * / < <= > >=))
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "(~a ~a ~a)" a-js op b-js))))

    ((primop-app '= a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "(~a == ~a)" a-js b-js))))

    ((primop-app 'remainder a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "(~a % ~a)" a-js b-js))))

    ((primop-app 'modulo a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "modulo(~a, ~a)" a-js b-js))))

    ((primop-app 'quotient a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "Math.floor(~a / ~a)" a-js b-js))))

    ((primop-app 'random)
     (return "(Math.random())"))

    ;; List primops.
    ((primop-app 'car l)
     (<- ((v (generate-js-node l)))
         (return (format "(~a.car)" v))))

    ((primop-app 'cdr l)
     (<- ((v (generate-js-node l)))
         (return (format "(~a.cdr)" v))))

    ((primop-app 'cons a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "makeCons(~a, ~a)" a-js b-js))))

    ;; Equality primops
    ((primop-app 'eq? a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "(~a == ~a)" a-js b-js))))

    ((primop-app 'equal? a b)
     (<- ((a-js (generate-js-node a))
          (b-js (generate-js-node b)))
         (return (format "(~a === ~a)" a-js b-js))))

    ;; IO primops
    ((primop-app 'display a)
     (<- ((v (generate-js-node a)))
         (return (format "display(~a)" v))))

    ;; Time primops
    ((primop-app 'current-milliseconds)
     (return "currentMilliseconds()"))

    ((primop-app 'delay-milliseconds ts)
     (<- ((v (generate-js-node ts)))
         (return (format "delayMilliseconds(~a)" v))))

    ;; Process primops
    ((primop-app 'make-uproc prio cont handler rtime state)
     (<- ((p (generate-js-node prio))
          (c (generate-js-node cont))
          (h (generate-js-node handler))
          (t (generate-js-node rtime))
          (s (generate-js-node state)))
         (return (format "makeUproc(~a, ~a, ~a, ~a, ~a)" p c h t s))))

    ((primop-app 'uproc-pid p)
     (<- ((v (generate-js-node p)))
         (return (format "(~a.pid)" v))))

    ((primop-app 'uproc-priority p)
     (<- ((v (generate-js-node p)))
         (return (format "(~a.priority)" v))))

    ((primop-app 'uproc-state p)
     (<- ((v (generate-js-node p)))
         (return (format "(~a.state)" v))))

    ((primop-app 'set-uproc-state! proc state)
     (<- ((p (generate-js-node proc))
          (s (generate-js-node state)))
         (return (format "setUprocState(~a, ~a)" p s))))

    ((primop-app 'uproc-rtime p)
     (<- ((v (generate-js-node p)))
         (return (format "(~a.rtime)" v))))

    ((primop-app 'set-uproc-rtime! proc ts)
     (<- ((p (generate-js-node proc))
          (t (generate-js-node ts)))
         (return (format "setUprocRTime(~a, ~a)" p t))))

    ((primop-app 'uproc-vtime p)
     (<- ((v (generate-js-node p)))
         (return (format "uprocVTime(~a)" v))))

    ((primop-app 'uproc-continuation p)
     (<- ((v (generate-js-node p)))
         (return (format "(~a.continuation)" v))))

    ((primop-app 'set-uproc-continuation! proc cont)
     (<- ((p (generate-js-node proc))
          (c (generate-js-node cont)))
         (return (format "setUprocContinuation(~a, ~a)" p c))))

    ((primop-app 'uproc-delimited-continuations p)
     (<- ((v (generate-js-node p)))
         (return (format "(~a.delimitedContinuations)" v))))

    ((primop-app 'set-uproc-delimited-continuations! proc cont)
     (<- ((p (generate-js-node proc))
          (c (generate-js-node cont)))
         (return (format "setUprocDelimitedContinuations(~a, ~a)" p c))))

    ((primop-app 'uproc-error-handler p)
     (<- ((v (generate-js-node p)))
         (return (format "(~a.errorHandler)" v))))

    ((primop-app 'set-uproc-error-handler! proc handler)
     (<- ((p (generate-js-node proc))
          (h (generate-js-node handler)))
         (return (format "setUprocErrorHandler(~a, ~a)" p h))))

    ((primop-app 'uproc-msg-queue-empty? p)
     (<- ((v (generate-js-node p)))
         (return (format "uprocMsgQueueEmpty(~a)" v))))

    ((primop-app 'uproc-enqueue-msg! proc msg)
     (<- ((p (generate-js-node proc))
          (m (generate-js-node msg)))
         (return (format "uprocEnqueueMsg(~a, ~a)" p m))))

    ((primop-app 'uproc-dequeue-msg! p)
     (<- ((v (generate-js-node p)))
         (return (format "uprocDequeueMsg(~a)" v))))

    ;; RBS primops
    ;; FIXME These should ideally not compile at all until the RBS is properly supported.
    ((primop-app 'assert! f)
     (<- ((v (generate-js-node f)))
         (return (format "assertFact(~a)" v))))

    ((primop-app 'signal! f)
     (<- ((v (generate-js-node f)))
         (return (format "signalFact(~a)" v))))

    ((primop-app 'retract! f)
     (<- ((v (generate-js-node f)))
         (return (format "retractFact(~a)" v))))

    ((primop-app 'select q)
     (<- ((v (generate-js-node q)))
         (return (format "selectFacts(~a)" v))))

    ((primop-app 'whenever-trampoline pat fun)
     (<- ((p (generate-js-node pat))
          (f (generate-js-node fun)))
         (return (format "wheneverTrampoline(~a, ~a)" p f))))

    ;; Closure primops
    ((primop-app '&make-closure env fun)
     (<- ((e (generate-js-node env))
          (f (generate-js-node fun)))
         (return (format "makeClosure(~a, ~a)" e f))))

    ((primop-app '&make-env args ...)
     (<- ((as (generate-js-sequence args)))
         (return (format "[~a]" (string-join as ", ")))))

    ((primop-app '&env-ref env offset)
     (<- ((e (generate-js-node env))
          (o (generate-js-node offset)))
         (return (format "(~a[~a])" e o))))

    ((primop-app '&set-env! env offset val)
     (<- ((e (generate-js-node env))
          (o (generate-js-node offset))
          (v (generate-js-node val)))
         (return (format "(~a[~a] = ~a)" e o v))))

    ((primop-app '&set-closure-env! clo env)
     (<- ((c (generate-js-node clo))
          (e (generate-js-node env)))
         (return (format "setClosureEnv(~a, ~a)" c e))))

    ((primop-app '&apply clo args ...)
     (let* ((local-closure (gensym '__closure)))
       (<- ((c (generate-js-node clo))
            (as (generate-js-sequence args)))
           (format "const ~a = ~a; ~a"
                   local-closure
                   c
                   (return
                    (format "(~a.fun(~a.env, ~a))"
                            local-closure
                            local-closure
                            (string-join as ", ")))))))

    ;; Continuation primops
    ((primop-app 'trampoline r)
     (<- ((v (generate-js-node r)))
         (return (format "trampoline(~a)" v))))

    ((primop-app 'suspend t)
     (<- ((v (generate-js-node t)))
         (return (format "suspend(~a)" v))))

    ((primop-app 'resumable? r)
     (<- ((v (generate-js-node r)))
         (return (format "isResumable(~a)" v))))

    ((primop-app 'resume r)
     (<- ((v (generate-js-node r)))
         (return (format "resume(~a)" v))))

    ((primop-app '&yield-cont-immediate kont hole)
     (<- ((k (generate-js-node kont))
          (h (generate-js-node hole)))
         (return (format "makeResumable(~a, ~a)" k h))))

    ((primop-app '&yield-cont kont hole)
     (let ((local-cont (gensym '__kont)))
       (<- ((k (generate-js-node kont))
            (h (generate-js-node hole)))
           (format "const ~a = ~a; if (__kontCounter-- > 0) { ~a } else { __kontCounter = ~a; ~a }"
                   local-cont
                   k
                   (return
                    (format "~a.fun(~a.env, ~a)"
                            local-cont
                            local-cont
                            h))
                   +js-continuation-hops+
                   (return
                    (format "makeResumable(~a, ~a)"
                            local-cont
                            h))))))

    ;; Modules & structures primops
    ((primop-app '&structure-binding name value)
     (<- ((n (generate-js-node name))
          (v (generate-js-node value)))
         (return (format "{name: ~a, value: ~a}" n v))))

    ((primop-app '&make-structure bindings ...)
     (<- ((bs (generate-js-sequence bindings)))
         (return (format "makeStructure([~a])" (string-join bs ", ")))))

    ((primop-app '&structure-ref structure name)
     (<- ((s (generate-js-node structure))
          (n (generate-js-node name)))
         (return (format "(~a[~a])" s n))))

    ((primop-app '&primitive-metadata meta ...)
     (return "null"))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))

(define (generate-js-sequence exprs return)
  (if (> (length exprs) 0)
      (generate-js-node (car exprs)
                        (lambda (first)
                          (generate-js-sequence (cdr exprs)
                                                (lambda (rest)
                                                  (return (cons first rest))))))
      (return '())))
