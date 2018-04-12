;; Target-safe variable renaming.

(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (mangle expr globals)
  (walk id
        (lambda (expr)
          (if (symbol? expr)
              (rename-symbol expr globals)
              expr))
        expr))

(define (rename-symbol symbol globals)
  ;; FIXME This shouldn't be here.
  (if (member symbol globals)
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
        ('else (string char))))
