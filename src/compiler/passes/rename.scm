;; Target-safe variable renaming.

(load "compiler/utils/utils.scm")
(load "compiler/utils/gensym.scm")

(load "compiler/env.scm")
(load "compiler/pass.scm")
(load "compiler/ast.scm")

(define symbol-rename
  (pass (schema "symbol-rename"
                'ast (ast-subset? '(const symbol
                                    if do let binding lambda primop-app)))
        (lambda (env)
          (env-update env 'ast mangle-names))))

(define (mangle-names expr)
  (case (get-type expr)
    ((const)
     expr)
    ((symbol)
     (if (equal? (ast-symbol-value expr) '_)
         (ast-update expr 'value (constantly (gensym 'WILD)))
         (ast-update expr 'value symbol->safe)))
    ((primop-app)
     (ast-update expr 'args (partial map mangle-names)))
    (else
     (walk-ast mangle-names expr))))

(define (symbol->safe s)
  (string->symbol (apply string-append
                         "__"
                         (map fix-char
                              (string->list (symbol->string s))))))

(define (fix-char char)
  ;; TODO Support arbitrary Unicode here.
  (cond ((equal? char #\*) "MULT")
        ((equal? char #\+) "PLUS")
        ((equal? char #\-) "_")
        ((equal? char #\_) "UNDER")
        ((equal? char #\/) "DIV")
        ((equal? char #\\) "BACK")
        ((equal? char #\|) "BAR")
        ((equal? char #\!) "BANG")
        ((equal? char #\?) "QUEST")
        ((equal? char #\=) "EQUAL")
        ((equal? char #\<) "LESS")
        ((equal? char #\>) "GREATER")
        ((equal? char #\%) "PROC")
        ((equal? char #\^) "CARET")
        ((equal? char #\&) "AMPER")
        ((equal? char #\@) "AT")
        ((equal? char #\$) "DOLLAR")
        ((equal? char #\~) "TYLDE")
        (else (string char))))