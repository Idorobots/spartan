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
  (format "let cc = (function(){~a})();~nwhile(typeof cc === \"object\" && typeof cc.kont === \"object\") {cc = cc.kont.fun(cc.kont.env, cc.hole)}"
          (generate-js-node generate-js-leaf expr)))

(define (generate-js-leaf value)
  (format "return ~a" value))

(define (generate-js-node return expr)
    (match-ast expr
      ((const v)
       (generate-js-node return v))

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

      ((if c t e)
       (format "if(~a){~a}else{~a}"
               (generate-js-node id c)
               (generate-js-node return t)
               (generate-js-node return e)))

      ((let bindings body)
       (foldr (lambda (b acc)
                (format "const ~a = ~a; ~a"
                        (generate-js-node id (ast-binding-var b))
                        (generate-js-node id (ast-binding-val b))
                        acc))
              (generate-js-node generate-js-leaf body)
              bindings))

      ((lambda args body)
       (format "((~a) => {~a})"
               (string-join (map (lambda (a)
                                   (generate-js-node id a))
                                 args)
                            ", ")
               (generate-js-node generate-js-leaf body)))

      ((do exprs ...)
       (let ((ret (last exprs))
             (statements (take exprs (- (length exprs) 1))))
         (format "~a; ~a"
                 (string-join (map (lambda (s)
                                     (generate-js-node id s))
                                   statements)
                              ";")
                 (generate-js-node return ret))))

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

      ((primop-app '> a b)
       (return
        (format "(~a > ~a)"
                (generate-js-node id a)
                (generate-js-node id b))))

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

      ((primop-app '&make-closure env fun)
       (return
        (format "{env:~a,fun:~a}"
                (generate-js-node id env)
                (generate-js-node id fun))))

      ((primop-app '&make-env args ...)
       (let ((args-js (map (lambda (a)
                             (generate-js-node id a))
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
        (format "~a.env = ~a"
                (generate-js-node id c)
                (generate-js-node id env))))

      ((primop-app '&apply c args ...)
       (let ((args-js (map (lambda (a)
                             (generate-js-node id a))
                           args))
             (closure (generate-js-node id c)))
         (return
          (format "(~a.fun(~a.env, ~a))"
                  closure
                  closure
                  (string-join args-js ", ")))))

      ((primop-app '&yield-cont k h)
       (let ((kont (generate-js-node id k))
             (hole (generate-js-node id h)))
         (format "if (__kontCounter-- > 0) { ~a } else { __kontCounter = ~a; ~a }"
                 (return
                  (format "~a.fun(~a.env, ~a)"
                          kont
                          kont
                          hole))
                 +js-continuation-hops+
                 (return
                  (format "{kont: ~a,hole:~a}"
                          kont
                          hole)))))

      ((primop-app op args ...)
       (let ((args-js (map (lambda (a)
                             (generate-js-node id a))
                           args)))
         (return
          (format "(~a(~a))"
                  (if (assoc op +js-primops+)
                      (symbol->safe op)
                      (compiler-bug "Unsupported primop:" op))
                  (string-join args-js ", ")))))

      ((primop-app op args ...)
       (compiler-bug "Unsupported primop:" op))

      (else
       (compiler-bug "Unsupported AST node type:" expr))))

(define +js-continuation-hops+ 100)

(define +js-primops+
  '((display . "(/*display*/function(v) { process.stdout.write(\"\"+v) })")
    (newline . "(/*newline*/function() { process.stdout.write(\"\\n\") })")
    (+ . "(/*+*/function(a, b) { return a + b })")
    (- . "(/*-*/function(a, b) { return a - b })")
    (* . "(/***/function(a, b) { return a * b })")
    (/ . "(/*/*/function(a, b) { return a / b })")
    (modulo . "(/*modulo*/function(a, b) { return a % b })")
    (quotient . "(/*quotient*/function(a, b) { return Math.floor(a/b) })")
    (< . "(/*<*/function(a, b) { return a < b })")
    (> . "(/*>*/function(a, b) { return a > b })")
    (= . "(/*=*/function(a, b) { return a = b })")
    (cons . "(/*cons*/function(a, b) { return { car: a, cdr: b} })")
    (car . "(/*car*/function(l) { return l.car })")
    (cdr . "(/*cdr*/function(l) { return l.cdr })")
    (list . "(/*list*/function(...vals) { return (vals.length() === 0) ? null : {car: vals[0], cdr: this(...vals.slice(1))} })")
    (nil? . "(/*nil?*/function(l) { return l === null })")
    (equal? . "(/*equal?*/function(a, b) { return a === b })")
    (not . "(/*not*/function(a) { return !a })")
    (ref . "(/*ref*/function(init) { return { ref: init } })")
    (assign! . "(/*assign!*/function(ref, val) { ref.ref = val })")
    (deref . "(/*deref*/function(ref) { return ref.ref })")
    (&make-closure . "(/*&make-closure*/function(env, fun) { return {env: env, fun: fun} })")
    (&set-closure-env! . "(/*&set-closure-env!*/function(c, env) { c.env = env })")
    (&make-env . "(/*&make-env*/function(...args) { return {args: args} })")
    (&env-ref . "(/*&env-ref*/function(env, offset) { return env.args[offset] })")
    (&apply . "(/*&apply*/function(c, ...args) { return c.fun(c.env, ...args) })")
    (&yield-cont . "(/*&yield-cont*/function(k, h) { return {kont: k, hole: h} })")))
