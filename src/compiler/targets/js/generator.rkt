#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/assets.rkt")
(require "../../utils/utils.rkt")

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
          (generate-js-node id value)))

(define (generate-js-node return expr)
  (match-ast expr
    ;; AST nodes
    ((string value)
     (return
      (format "~v" value)))

    ((list vals ...)
     (return
      (foldr (lambda (v acc)
              (format "{car:~a,cdr:~a}"
                      (generate-js-node id v)
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
     (generate-js-node return v))

    ((lambda args body)
     (format "((~a) => {~a})"
             (string-join (map (partial generate-js-node id)
                               args)
                          ", ")
             (generate-js-node (lambda (v)
                                 (format "return ~a" v))
                               body)))

    ((if c t e)
     ;; FIXME Booleans should be treated according to the language semantics.
     (format "if (~a) { ~a } else { ~a }"
             (generate-js-node id c)
             (generate-js-node return t)
             (generate-js-node return e)))

    ((let bindings body)
     (foldr (lambda (b acc)
              (format "const ~a = ~a; ~a"
                      (generate-js-node id (ast-binding-var b))
                      (generate-js-node id (ast-binding-val b))
                      acc))
            (generate-js-node return body)
            bindings))

    ((do exprs ...)
     (let* ((last-s (last exprs))
            (statements (take exprs (- (length exprs) 1)))
            (ret (generate-js-node return last-s)))
       (format "~a; ~a"
               (string-join (map (partial generate-js-node id)
                                 statements)
                            ";")
               ret)))

    ;; Math primops
    ((primop-app '+ a b)
     (return
      (format "(~a + ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '- a b)
     (return
      (format "(~a - ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '* a b)
     (return
      (format "(~a * ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '/ a b)
     (return
      (format "(~a / ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '= a b)
     (return
      (format "(~a == ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '< a b)
     (return
      (format "(~a < ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '<= a b)
     (return
      (format "(~a <= ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '> a b)
     (return
      (format "(~a > ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app '>= a b)
     (return
      (format "(~a >= ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app 'modulo a b)
     (return
      (format "(~a % ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app 'quotient a b)
     (return
      (format "(Math.floor(~a / ~a))"
              (generate-js-node id a)
              (generate-js-node id b))))

    ;; List primops.
    ((primop-app 'car l)
     (return
      (format "(~a.car)"
              (generate-js-node id l))))

    ((primop-app 'cdr l)
     (return
      (format "(~a.cdr)"
              (generate-js-node id l))))

    ((primop-app 'cons a b)
     (return
      (format "{car:~a,cdr:~a}"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app 'list)
     (return "null"))

    ((primop-app 'list args ...)
     (return (foldr (lambda (a acc)
                      (format "{car:~a,cdr:~a}"
                              (generate-js-node id a)
                              acc))
                    "null"
                    args)))

    ((primop-app 'append a b)
     (return
      (format "(function() { const app = ((rest, tail) => (rest === null) ? tail : {car: rest.car, cdr: app(rest.cdr, tail)}); return app(~a, ~a)})()"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app 'nil? l)
     (return
      (format "(~a === null)"
              (generate-js-node id l))))

    ;; Boolean primops
    ((primop-app 'eq? a b)
     (return
      (format "(~a == ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app 'equal? a b)
     (return
      (format "(~a === ~a)"
              (generate-js-node id a)
              (generate-js-node id b))))

    ((primop-app 'not a)
     (return
      (format "(!~a)"
              (generate-js-node id a))))

    ;; IO primops
    ((primop-app 'display a)
     (return
      (format "(__write(~a), null)"
              (generate-js-node id a))))

    ((primop-app 'newline)
     (return "(__write(\"\\n\"), null)"))

    ;; Mutable primops
    ((primop-app 'ref a)
     (return
      (format "{ref: ~a}"
              (generate-js-node id a))))

    ((primop-app 'deref a)
     (return
      (format "(~a.ref)"
              (generate-js-node id a))))

    ((primop-app 'assign! a b)
     (let ((a-js (generate-js-node id a))
           (b-js (generate-js-node id b)))
       (return
        (format "((~a.ref = ~a), ~a)" a-js b-js a-js))))

    ;; Closure & continuation primops.
    ((primop-app '&make-closure env fun)
     (return
      (format "{env:~a,fun:~a}"
              (generate-js-node id env)
              (generate-js-node id fun))))

    ((primop-app '&make-env args ...)
     (let ((args-js (map (partial generate-js-node id)
                         args)))
       (return
        (format "[~a]"
                (string-join args-js ", ")))))

    ((primop-app '&env-ref env offset)
     (return
      (format "(~a[~a])"
              (generate-js-node id env)
              (generate-js-node id offset))))

    ((primop-app '&set-closure-env! c env)
     (return
      (format "(~a.env = ~a)"
              (generate-js-node id c)
              (generate-js-node id env))))

    ((primop-app '&apply c args ...)
     (let* ((args-js (map (partial generate-js-node id)
                          args))
            (variant (cond ((= (length args) 1)
                            "1")
                           ((= (length args) 2)
                            "2")
                           ((= (length args) 3)
                            "3")
                           (else
                            "N"))))
       (generate-js-node
        (lambda (closure)
          (return
           (format "(__apply~a(~a, ~a))"
                   variant
                   closure
                   (string-join args-js ", "))))
        c)))

    ((primop-app '&yield-cont k h)
     (generate-js-node
      (lambda (kont)
        (format "if (__kontCounter-- > 0) { ~a } else { __kontCounter = ~a; ~a }"
                (generate-js-node
                 (lambda (v)
                   (return
                    (format "__apply_cont(~a, ~a)"
                            kont
                            v)))
                 h)
                +js-continuation-hops+
                (generate-js-node
                 (lambda (v)
                   (return
                    (format "{kont: ~a,hole:~a}"
                            kont
                            v)))
                 h)))
      k))

    ;; Modules & structures primops
    ((primop-app '&structure-binding name value)
     (return
      (format "{name: ~a, value: ~a}"
              (generate-js-node id name)
              (generate-js-node id value))))

    ((primop-app '&make-structure bindings ...)
     (let ((bindings-js (map (partial generate-js-node id)
                             bindings)))
       (return
        (format "(function() {const __s = { struct: true }; [~a].map((b) => __s[b.name] = b.value); return __s})()"
                (string-join bindings-js ", ")))))

    ((primop-app '&structure-ref s name)
     (return
      (format "(~a[~a])"
              (generate-js-node id s)
              (generate-js-node id name))))

    ((primop-app op args ...)
     (compiler-bug "Unsupported primop:" op))

    (else
     (compiler-bug "Unsupported AST node type:" expr))))
