#lang racket

(require "../../env.rkt")
(require "../../ast.rkt")
(require "../../errors.rkt")
(require "../../utils/assets.rkt")
(require "../../utils/gensym.rkt")
(require "../../utils/utils.rkt")
(require "../../passes/rename.rkt")

(provide generate-js)

(define +js-continuation-hops+ 100)
(define +js-bootstrap+ (embed-file-contents "./bootstrap.js"))

(define (generate-js env)
  ;; Generate JavaScript code for the root node
  (string-append
   +js-bootstrap+
   (foldr (lambda (v acc)
            (string-append
             (generate-js-const (car v) (cdr v))
             acc))
          (generate-js-root (env-get env 'init))
          (env-get env 'data))))

(define (generate-js-const name value)
  (generate-js-node
   (lambda (v)
     (format "const ~a = ~a;~n" name v))
   value))

(define (generate-js-root expr)
  ;; NOTE Needed to avoid stack overflow due to continuations.
  (format "let __kontCounter = ~a;
let cc = (function(){~a})();
while(cc !== __nil && typeof cc === \"object\" && typeof cc.kont === \"function\") {
  cc = cc.kont(cc.hole)
}"
          +js-continuation-hops+
          (generate-js-node generate-js-leaf expr)))

(define (generate-js-leaf value)
  (format "return ~a" value))

(define (generate-js-args return args)
  (define (gen args acc)
    (if (empty? args)
        (return (reverse acc))
        (generate-js-node (lambda (a)
                            (gen (cdr args) (cons a acc)))
                          (car args))))
  (gen args '()))

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
              "__nil"
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

      ((lambda (env args ...) body)
       (return
        (format "(function (~a) { const ~a = this.env; ~a })"
                (string-join (map (partial generate-js-node id)
                                  args)
                             ", ")
                (generate-js-node id env)
                (generate-js-node generate-js-leaf body))))

      ((if c t e)
       ;; FIXME Booleans should be treated according to the language semantics.
       (format "if (~a) { ~a } else { ~a }"
               (generate-js-node id c)
               (generate-js-node return t)
               (generate-js-node return e)))

      ((let bindings body)
       (foldr (lambda (b acc)
                (generate-js-node
                 (lambda (var)
                   (generate-js-node
                    (lambda (val)
                      (format "const ~a = ~a; ~a" var val acc))
                    (ast-binding-val b)))
                 (ast-binding-var b)))
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
       (return "__nil"))

      ((primop-app 'list args ...)
       (return (foldr (lambda (a acc)
                        (format "{car:~a,cdr:~a}"
                                (generate-js-node id a)
                                acc))
                      "__nil"
                      args)))

      ((primop-app 'append a b)
       (return
        (format "(function() { const app = ((rest, tail) => (rest === __nil) ? tail : {car: rest.car, cdr: app(rest.cdr, tail)}); return app(~a, ~a)})()"
                (generate-js-node id a)
                (generate-js-node id b))))

      ((primop-app 'nil? l)
       (return
        (format "(~a === __nil)"
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
        (format "(__write(~a), __nil)"
                (generate-js-node id a))))

      ((primop-app 'newline)
       (return "(__write(\"\\n\"), __nil)"))

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
       (let ((tmp-env (symbol->safe (gensym 'env)))
             (tmp (symbol->safe (gensym 'tmp))))
         (generate-js-node
          (lambda (e)
            (generate-js-node
             (lambda (f)
               (format "const ~a = {env: ~a}; const ~a = ~a.bind(~a); ~a.env = ~a; ~a"
                       tmp-env
                       e
                       tmp
                       f
                       tmp-env
                       tmp
                       tmp-env
                       (return (symbol->string tmp))))
             fun))
          env)))

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
        (format "(~a.env.env = ~a)"
                (generate-js-node id c)
                (generate-js-node id env))))

      ((primop-app '&apply c args ...)
       (generate-js-args
        (lambda (args-js)
          (generate-js-node
           (lambda (closure)
             (return
              (format "(~a(~a))"
                      closure
                      (string-join args-js ", "))))
           c))
        args))

      ((primop-app '&yield-cont k h)
       (generate-js-node
        (lambda (kont)
          (format "if (__kontCounter-- > 0) { ~a } else { __kontCounter = ~a; ~a }"
                  (generate-js-node
                   (lambda (v)
                     (return
                      (format "~a(~a)"
                              kont
                              v)))
                   h)
                  +js-continuation-hops+
                  (generate-js-node
                   (lambda (v)
                     (return
                      (format "{kont:~a,hole:~a}"
                              kont
                              v)))
                   h)))
        k))

      ;; Modules & structures primops
      ((primop-app '&structure-binding name value)
       (generate-js-node
        (lambda (n)
          (generate-js-node
           (lambda (v)
             (return
              (format "{name: ~a, value: ~a}" n v)))
           value))
        name))

      ((primop-app '&make-structure bindings ...)
       (let ((tmp-s (symbol->safe (gensym 's))))
         (generate-js-args
          (lambda (bindings-js)
            (format "const ~a = { struct: true }; [~a].map((b) => ~a[b.name] = b.value); ~a"
                    tmp-s
                    (string-join bindings-js ", ")
                    tmp-s
                    (return (symbol->string tmp-s))))
          bindings)))

      ((primop-app '&structure-ref s name)
       (return
        (format "(~a[~a])"
                (generate-js-node id s)
                (generate-js-node id name))))

      ((primop-app op args ...)
       (compiler-bug "Unsupported primop:" op))

      (else
       (compiler-bug "Unsupported AST node type:" expr))))
