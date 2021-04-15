;; Target-safe variable renaming.

(load "compiler/utils/utils.scm")

(load "compiler/env.scm")
(load "compiler/ast.scm")

(define symbol-rename
  (pass (schema 'ast (ast-subset? '(quote number symbol string list
                                    if do let fix binding lambda primop-app)))
        (lambda (env)
          (env-update env 'ast mangle-names))))

(define (mangle-names expr)
  (case (get-type expr)
    ((quote)
     expr)
    ((symbol)
     (ast-update expr 'value symbol->safe))
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
