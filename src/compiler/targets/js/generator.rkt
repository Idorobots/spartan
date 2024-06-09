#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/assets.rkt")
(require "../../utils/utils.rkt")
(require "../../utils/gensym.rkt")

(provide generate-js)

(define +js-continuation-hops+ 200)
(define +js-bootstrap+ (embed-file-contents "./bootstrap.js"))
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

(define (generate-js-node expr return)
  (match-ast expr
    ;; AST nodes
    ((string value)
     (return
      (format "~v" value)))

    ((list vals ...)
     (return
      (foldr (lambda (v acc)
              (format "{car:~a,cdr:~a}"
                      (generate-js-node v id)
                      acc))
            "null"
            vals)))

    ((number v)
     (return
      (format "~a" v)))

    ((symbol s)
     (return
      (format "~a" s)))

    ((const (symbol s))
     (return
      (format "\"~a\"" s)))

    ((const v)
     (generate-js-node v return))

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

    ((primop-app 'modulo a b)
     (return
      (format "(~a % ~a)"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'quotient a b)
     (return
      (format "(Math.floor(~a / ~a))"
              (generate-js-node a id)
              (generate-js-node b id))))

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
      (format "{car:~a,cdr:~a}"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'list)
     (return "null"))

    ((primop-app 'list args ...)
     (return (foldr (lambda (a acc)
                      (format "{car:~a,cdr:~a}"
                              (generate-js-node a id)
                              acc))
                    "null"
                    args)))

    ((primop-app 'append a b)
     (return
      (format "(function() { const app = ((rest, tail) => (rest === null) ? tail : {car: rest.car, cdr: app(rest.cdr, tail)}); return app(~a, ~a)})()"
              (generate-js-node a id)
              (generate-js-node b id))))

    ((primop-app 'nil? l)
     (return
      (format "(~a === null)"
              (generate-js-node l id))))

    ;; Boolean primops
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

    ((primop-app 'not a)
     (return
      (format "(!~a)"
              (generate-js-node a id))))

    ;; IO primops
    ((primop-app 'display a)
     (return
      (format "(__write(~a), null)"
              (generate-js-node a id))))

    ((primop-app 'newline)
     (return "(__write(\"\\n\"), null)"))

    ;; Mutable primops
    ((primop-app 'ref a)
     (return
      (format "{ref: ~a}"
              (generate-js-node a id))))

    ((primop-app 'deref a)
     (return
      (format "(~a.ref)"
              (generate-js-node a id))))

    ((primop-app 'assign! a b)
     (let ((a-js (generate-js-node a id))
           (b-js (generate-js-node b id)))
       (return
        (format "((~a.ref = ~a), ~a)" a-js b-js a-js))))

    ;; Closure & continuation primops.
    ((primop-app '&make-closure env fun)
     (return
      (format "{env:~a,fun:~a}"
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

    ((primop-app '&set-closure-env! c env)
     (return
      (format "(~a.env = ~a)"
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
                   ;; FIXME Some tests indicate that this is actually slower than calling into __apply*(). Likely JIT shenanigans.
                   (format "(~a.fun(~a.env, ~a))"
                           local-closure
                           local-closure
                           (string-join args-js ", "))))))))

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
                      (format "{kont: ~a,hole:~a}"
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
        (format "(function() {const __s = { struct: true }; [~a].map((b) => __s[b.name] = b.value); return __s})()"
                (string-join bindings-js ", ")))))

    ((primop-app '&structure-ref s name)
     (return
      (format "(~a[~a])"
              (generate-js-node s id)
              (generate-js-node name id))))

    ((primop-app op args ...)
     (compiler-bug "Unsupported primop:" op))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))
