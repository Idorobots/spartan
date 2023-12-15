#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")
(require "../utils/utils.rkt")
(require "rename.rkt")

(provide generate-target-code)

(define generate-target-code
  (pass (schema "generate-target-code"
                'data (list-of? (a-pair? a-symbol?
                                         (ast-subset? '(const symbol if do let binding lambda primop-app))))
                'init (ast-subset? '(const symbol if do let binding primop-app))
                'target a-symbol?)
        (lambda (env)
          (case (env-get env 'target)
            ((ECMAScript6)
             (generate-js env))
            (else
             (generate-scheme env))))))

(define (generate-scheme env)
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
                ,init))))

(define +js-continuation-hops+ 100)

(define +js-primops+
  '((debug . "false")
    (nil . "null")
    (display . "{fun: (function(e, v, c) { process.stdout.write(JSON.stringify(v)); return {kont: c, hole: null} })}")
    (newline . "{fun: (function(e, c) { process.stdout.write(\"\\n\"); return {kont: c, hole: null} })}")
    (+ . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a + b) } })}")
    (- . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a - b) } })}")
    (* . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a * b) } })}")
    (/ . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a / b) } })}")
    (modulo . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a % b) } })}")
    (quotient . "{fun: (function(e, a, b, c) { return {kont: c, hole: Math.floor(a/b) } })}")
    (< . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a < b) } })}")
    (<= . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a <= b) } })}")
    (> . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a > b) } })}")
    (>= . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a >= b) } })}")
    (= . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a == b) } })}")
    (cons . "{fun: (function(e, a, b, c) { return {kont: c, hole: { car: a, cdr: b} } })}")
    (car . "{fun: (function(e, l, c) { return {kont: c, hole: l.car } })}")
    (cdr . "{fun: (function(e, l, c) { return {kont: c, hole: l.cdr } })}")
    (list . "{fun: (function(e, ...args) { const c = args[args.length - 1]; const vals = args.slice(0, args.length - 1); const lst = ((rest) => (rest.length === 0) ? null : {car: rest[0], cdr: lst(rest.slice(1))}); return {kont: c, hole: lst(vals)} })}")
    (append "{fun: (function (e, a, b, c) { const app = ((rest) => (rest === null) ? b : {car: rest.car, cdr: app(rest.cdr)}); return {kont: c, hole: app(a) } })}")
    (nil? . "{fun: (function(e, l, c) { return {kont: c, hole: (l === null) } })}")
    (eq? . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a == b) } })}")
    (equal? . "{fun: (function(e, a, b, c) { return {kont: c, hole: (a === b) } })}")
    (not . "{fun: (function(e, a, c) { return {kont: c, hole: (a === null) } })}")
    (ref . "{fun: (function(e, init, c) { return {kont: c, hole: { ref: init } } })}")
    (assign! . "{fun: (function(e, r, val, c) { r.ref = val; return {kont: c, hole: r }})}")
    (deref . "{fun: (function(e, r, c) { return {kont: c, hole: r.ref }})}")
    (call/current-continuation . "{fun: (function(e, f, c) { return f.fun(f.env, {env: null, fun: (e, ret, _) => ({kont: c, hole: ret}) }, c)})}")
    ;; TODO Rely on continuations instead.
    (raise . "{fun: (function(e, ex, c) { throw ex })}")))

(define (generate-js env)
  ;; Generate JavaScript code for the root node
  (string-append
   (foldr (lambda (v acc)
            (string-append
             (format "const ~a = ~a;~n"
                     (symbol->safe (car v))
                     (cdr v))
             acc))
          (format "let __kontCounter = ~a;~n" +js-continuation-hops+)
          +js-primops+)
   (foldr (lambda (v acc)
            (string-append
             (generate-js-const (car v) (cdr v))
             acc))
          (generate-js-root (env-get env 'init))
          (env-get env 'data))))

(define (generate-js-const name value)
  (format "const ~a = ~a;~n"
          name
          (generate-js-node id value)))

(define (generate-js-root expr)
  ;; NOTE Needed to avoid stack overflow due to continuations.
  (format "let cc = (function(){~a})();~nwhile(cc !== null && typeof cc === \"object\" && typeof cc.kont === \"object\") { if(__debug) console.log(cc); cc = cc.kont.fun(cc.kont.env, cc.hole)}"
          (generate-js-node generate-js-leaf expr)))

(define (generate-js-leaf value)
  (format "return ~a" value))

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
               (generate-js-node generate-js-leaf body)))

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
        (format "(~a === null)"
                (generate-js-node id a))))

      ;; IO primops
      ((primop-app 'display a)
       (return
        (format "(process.stdout.write(JSON.stringify(~a)), null)"
                (generate-js-node id a))))

      ((primop-app 'newline)
       (return "(process.stdout.write(\"\\n\"), null)"))

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
       (let ((args-js (map (partial generate-js-node id)
                           args)))
         (generate-js-node
          (lambda (closure)
                             (return
                              (format "(~a.fun(~a.env, ~a))"
                                      closure
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
                      (format "~a.fun(~a.env, ~a)"
                              kont
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
          (format "(function() {const __s = {}; [~a].map((b) => __s[b.name] = b.value); return __s})()"
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
