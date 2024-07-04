#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/assets.rkt")
(require "../../utils/utils.rkt")
(require "../../utils/gensym.rkt")

(provide generate-js)

(define +js-continuation-hops+ 1)
(define +js-bootstrap+ (embed-file-contents "./bootstrap.js"))
(define +core-bootstrap+ (embed-file-contents "./core.js"))
(define +js-runtime+ (embed-file-contents "./rt.js"))

(define (generate-js env)
  ;; Generate JavaScript code for the root node
  (let* ((init (env-get env 'init))
         (init-loc (ast-node-location init))
         (defs (env-get env 'data))
         (unknown-loc (location 0 0)))
    (string-append
     (generate-js-def '__rt_continuation_hops
                      (generated
                       (make-ast-const unknown-loc
                                       (make-ast-number unknown-loc
                                                        +js-continuation-hops+))))
     +js-bootstrap+
     +core-bootstrap+
     (foldr (lambda (v acc)
              (string-append
               (generate-js-def (car v) (cdr v))
               acc))
            ""
            defs)
     (generate-js-def '__module_init
                      (generated
                       (make-ast-lambda init-loc '() init)))
     +js-runtime+)))

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
     (format "((~a) => {~a})"
             (string-join (map (lambda (a)
                                 (generate-js-node a id))
                               args)
                          ", ")
             (generate-js-node body
                               (lambda (v)
                                 (format "return ~a" v)))))

    ((if c t e)
     ;; FIXME Booleans should be treated according to the language semantics.
     (format "if (~a) { ~a } else { ~a }"
             (generate-js-node c id)
             (generate-js-node t return)
             (generate-js-node e return)))

    ((let bindings body)
     (foldr (lambda (b acc)
              (format "const ~a = ~a; ~a"
                      (generate-js-node (ast-binding-var b) id)
                      (generate-js-node (ast-binding-val b) id)
                      acc))
            (generate-js-node body return)
            bindings))

    ((do exprs ...)
     (let* ((last-s (last exprs))
            (statements (take exprs (- (length exprs) 1)))
            (ret (generate-js-node last-s return)))
       (format "~a; ~a"
               (string-join (map (lambda (s)
                                   (generate-js-node s id))
                                 statements)
                            ";")
               ret)))

    ;; Mutable primops
    ((primop-app 'ref a)
     (return
      (format "makeRef(~a)"
              (generate-js-node a id))))

    ((primop-app 'deref a)
     (return
      (format "deref(~a)"
              (generate-js-node a id))))

    ((primop-app 'assign! a b)
     (let ((a-js (generate-js-node a id))
           (b-js (generate-js-node b id)))
       (return
        (format "assign(~a, ~a)" a-js b-js))))

    ;; Math primops
    ((primop-app op a b)
     #:when (member op '(+ - * / < <= > >=))
     (return
      (format "(~a ~a ~a)"
              (generate-js-node a id)
              op
              (generate-js-node b id))))

    ((primop-app '= a b)
     (return
      (format "(~a == ~a)"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'remainder a b)
     (return
      (format "(~a % ~a)"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'modulo a b)
     (return
      (format "modulo(~a, ~a)"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'quotient a b)
     (return
      (format "(Math.floor(~a / ~a))"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'random)
     (return "(Math.random())"))

    ;; List primops.
    ((primop-app 'car l)
     (return
      (format "(~a.car)"
              (generate-js-node l id))))

    ((primop-app 'cdr l)
     (return
      (format "(~a.cdr)"
              (generate-js-node l id))))

    ((primop-app 'cons a b)
     (return
      (format "makeCons(~a, ~a)"
              (generate-js-node a id)
              (generate-js-node b id))))

    ;; Equality primops
    ((primop-app 'eq? a b)
     (return
      (format "(~a == ~a)"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'equal? a b)
     (return
      (format "(~a === ~a)"
              (generate-js-node a id)
              (generate-js-node b id))))

    ;; IO primops
    ((primop-app 'display a)
     (return
      (format "display(~a)"
              (generate-js-node a id))))

    ;; Time primops
    ((primop-app 'current-milliseconds)
     (return "currentMilliseconds()"))

    ((primop-app 'delay-milliseconds ts)
     (return
      (format "delayMilliseconds(~a)"
              (generate-js-node ts id))))

    ;; Process primops
    ((primop-app 'make-uproc prio cont handler rtime state)
     (return
      (format "makeUproc(~a, ~a, ~a, ~a, ~a)"
              (generate-js-node prio id)
              (generate-js-node cont id)
              (generate-js-node handler id)
              (generate-js-node rtime id)
              (generate-js-node state id))))

    ((primop-app 'uproc-pid p)
     (return
      (format "(~a.pid)"
              (generate-js-node p id))))

    ((primop-app 'uproc-priority p)
     (return
      (format "(~a.priority)"
              (generate-js-node p id))))

    ((primop-app 'uproc-state p)
     (return
      (format "(~a.state)"
              (generate-js-node p id))))

    ((primop-app 'set-uproc-state! p s)
     (return
      (format "setUprocState(~a, ~a)"
              (generate-js-node p id)
              (generate-js-node s id))))

    ((primop-app 'uproc-rtime p)
     (return
      (format "(~a.rtime)"
              (generate-js-node p id))))

    ((primop-app 'set-uproc-rtime! p ts)
     (return
      (format "setUprocRtime(~a, ~a)"
              (generate-js-node p id)
              (generate-js-node ts id))))

    ((primop-app 'uproc-vtime p)
     (return
      (format "uprocVTime(~a)"
              (generate-js-node p id))))

    ((primop-app 'uproc-continuation p)
     (return
      (format "(~a.continuation)"
              (generate-js-node p id))))

    ((primop-app 'set-uproc-continuation! p c)
     (return
      (format "setUprocContinuation(~a, ~a)"
              (generate-js-node p id)
              (generate-js-node c id))))

    ((primop-app 'uproc-delimited-continuations p)
     (return
      (format "(~a.delimitedContinuations)"
              (generate-js-node p id))))

    ((primop-app 'set-uproc-delimited-continuations! p c)
     (return
      (format "setUprocDelimitedContinuations(~a, ~a)"
              (generate-js-node p id)
              (generate-js-node c id))))

    ((primop-app 'uproc-error-handler p)
     (return
      (format "(~a.errorHandler)"
              (generate-js-node p id))))

    ((primop-app 'set-uproc-error-handler! p h)
     (return
      (format "setUprocErrorHandler(~a, ~a)"
              (generate-js-node p id)
              (generate-js-node h id))))

    ((primop-app 'uproc-msg-queue-empty? p)
     (return
      (format "uprocMsgQueueEmpty(~a)"
              (generate-js-node p id))))

    ((primop-app 'uproc-enqueue-msg! p m)
     (return
      (format "uprocEnqueueMsg(~a, ~a)"
              (generate-js-node p id)
              (generate-js-node m id))))

    ((primop-app 'uproc-dequeue-msg! p)
     (return
      (format "uprocDequeueMsg(~a)"
              (generate-js-node p id))))

    ;; RBS primops
    ;; TODO These should ideally not compile at all until the RBS is properly supported.
    ((primop-app 'assert! f)
     (return
      (format "assertFact(~a)"
              (generate-js-node f id))))

    ((primop-app 'signal! f)
     (return
      (format "signalFact(~a)"
              (generate-js-node f id))))

    ((primop-app 'retract! f)
     (return
      (format "retractFact(~a)"
              (generate-js-node f id))))

    ((primop-app 'select q)
     (return
      (format "selectFacts(~a)"
              (generate-js-node q id))))

    ((primop-app 'whenever-trampoline p f)
     (return
      (format "wheneverTrampoline(~a, ~a)"
              (generate-js-node p id)
              (generate-js-node f id))))

    ;; Closure primops
    ((primop-app '&make-closure env fun)
     (return
      (format "makeClosure(~a, ~a)"
              (generate-js-node env id)
              (generate-js-node fun id))))

    ((primop-app '&make-env args ...)
     (let ((args-js (map (lambda (a)
                           (generate-js-node a id))
                         args)))
       (return
        (format "[~a]"
                (string-join args-js ", ")))))

    ((primop-app '&env-ref env offset)
     (return
      (format "(~a[~a])"
              (generate-js-node env id)
              (generate-js-node offset id))))

    ((primop-app '&set-env! env offset val)
     (return
      (format "(~a[~a] = ~a)"
              (generate-js-node env id)
              (generate-js-node offset id)
              (generate-js-node val id))))

    ((primop-app '&set-closure-env! c env)
     (return
      (format "setClosureEnv(~a, ~a)"
              (generate-js-node c id)
              (generate-js-node env id))))

    ((primop-app '&apply c args ...)
     (let* ((args-js (map (lambda (a)
                            (generate-js-node a id))
                          args))
            (local-closure (gensym '__closure)))
       (generate-js-node
        c
        (lambda (closure)
          (format "const ~a = ~a; ~a"
                  local-closure
                  closure
                  (return
                   (format "(~a.fun(~a.env, ~a))"
                           local-closure
                           local-closure
                           (string-join args-js ", "))))))))

    ;; Continuation primops
    ((primop-app 'trampoline r)
     (return
      (format "trampoline(~a)"
              (generate-js-node r id))))

    ((primop-app 'suspend t)
     (return
      (format "suspend(~a)"
              (generate-js-node t id))))

    ((primop-app 'resumable? r)
     (return
      (format "isResumable(~a)"
              (generate-js-node r id))))

    ((primop-app 'resume r)
     (return
      (format "resume(~a)"
              (generate-js-node r id))))

    ((primop-app '&yield-cont-immediate k h)
     (generate-js-node
      k
      (lambda (kont)
        (generate-js-node
         h
         (lambda (v)
           (return
            (format "makeResumable(~a, ~a)"
                    kont
                    v)))))))

    ((primop-app '&yield-cont k h)
     (let ((local-cont (gensym '__kont)))
       (generate-js-node
        k
        (lambda (kont)
          (generate-js-node
           h
           (lambda (v)
             (format "const ~a = ~a; if (__kontCounter-- > 0) { ~a } else { __kontCounter = ~a; ~a }"
                     local-cont
                     kont
                     (return
                      (format "~a.fun(~a.env, ~a)"
                              local-cont
                              local-cont
                              v))
                     +js-continuation-hops+
                     (return
                      (format "makeResumable(~a, ~a)"
                              local-cont
                              v)))))))))

    ;; Modules & structures primops
    ((primop-app '&structure-binding name value)
     (return
      (format "{name: ~a, value: ~a}"
              (generate-js-node name id)
              (generate-js-node value id))))

    ((primop-app '&make-structure bindings ...)
     (let ((bindings-js (map (lambda (b)
                               (generate-js-node b id))
                             bindings)))
       (return
        (format "makeStructure([~a])"
                (string-join bindings-js ", ")))))

    ((primop-app '&structure-ref s name)
     (return
      (format "(~a[~a])"
              (generate-js-node s id)
              (generate-js-node name id))))

    ((primop-app '&primitive-metadata meta ...)
     (return "null"))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))
