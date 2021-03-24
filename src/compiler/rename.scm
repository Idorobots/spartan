;; Target-safe variable renaming.

(load "compiler/utils/utils.scm")

(load "compiler/ast.scm")

(define (mangle expr)
  (walk id
        (lambda (expr)
          (if (symbol? expr)
              (rename-symbol expr)
              expr))
        expr))

(define (rename-symbol symbol)
  (if (primop? symbol)
      symbol
      (symbol->safe symbol)))

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
