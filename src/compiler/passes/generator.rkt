#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../errors.rkt")
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
            ((js)
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
  (display
   (foldr (lambda (v acc)
            (string-append
             (format "const ~a = ~a;~n"
                     (symbol->safe (car v))
                     (cdr v))
             acc))
          ""
          +js-primops+))
  (displayln
   (foldr (lambda (v acc)
            (string-append
             (generate-js-const (car v) (cdr v))
             acc))
          (generate-js-root (env-get env 'init))
          (env-get env 'data))))

(define (generate-js-const name value)
  (format "const ~a = ~a;~n"
          name
          (generate-js-node value)))

(define (generate-js-root expr)
  ;; NOTE Needed to avoid stack overflow due to continuations.
  (format "let cc = ~a;~nwhile(typeof cc === \"object\" && typeof cc.kont === \"object\") {console.log(cc)~ncc = cc.kont.fun(cc.kont.env, cc.hole)~n}"
          (generate-js-node expr)))

(define (generate-js-node expr)
    (match-ast expr
      ((const v)
       (format "~a" (generate-js-node v)))

      ((string value)
       (format "~v" value))

      ((list vals ...)
       (foldr (lambda (v acc)
                (format "{ car: ~a, cdr: ~a}"
                        (generate-js-node v)
                        acc))
              "null"
              vals))

      ((number v)
       (format "~a" v))

      ((symbol s)
       (format "~a" s))

      ((if c t e)
       (format "((~a)?(~a):(~a))~n"
               (generate-js-node c)
               (generate-js-node t)
               (generate-js-node e)))

      ((let bindings body)
       (let ((vars-js (map (lambda (b)
                             (generate-js-node (ast-binding-var b)))
                           bindings))
             (vals-js (map (lambda (b)
                             (generate-js-node (ast-binding-val b)))
                           bindings))
             (body-js (generate-js-node body)))
         (format "(/*let*/function(~a){ return ~a })(~a)"
                 (string-join vars-js ", ")
                 body-js
                 (string-join vals-js ", "))))

      ((lambda args body)
       (format "(/*lambda*/function (~a) { return ~a })"
               (string-join (map generate-js-node args)
                            ", ")
               (generate-js-node body)))

      ((do exprs ...)
       (string-join (map generate-js-node exprs)
                    ", "))

      ((primop-app op args ...)
       (let ((args-js (map generate-js-node args)))
         (format "(~a(~a))"
                 (if (assoc op +js-primops+)
                     (symbol->safe op)
                     (compiler-bug "Unsupported primop:" op))
                 (string-join args-js ", "))))

      (else
       (compiler-bug "Unsupported node type:" expr))))

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
