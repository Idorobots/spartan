#lang racket

;; Target-safe variable renaming.

(require "../utils/utils.rkt")
(require "../utils/gensym.rkt")

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(provide symbol-rename
         ;; FIXME For test access.
         mangle-names symbol->safe)

(define symbol-rename
  (pass (schema "symbol-rename"
                'data (list-of? (a-pair? a-symbol?
                                         (ast-subset? '(const symbol if do let binding lambda primop-app))))
                'init (ast-subset? '(const symbol if do let binding primop-app)))
        (lambda (env)
          (env-update (env-update env 'init mangle-names)
                      'data (lambda (values)
                              (map (lambda (v)
                                     (cons (symbol->safe (car v))
                                           (mangle-names (cdr v))))
                                   values))))))

(define (mangle-names expr)
  (match-ast expr
   ((const _)
    expr)
   ((symbol '_)
    (set-ast-symbol-value expr (gensym '__WILD)))
   ((symbol _)
    (set-ast-symbol-value expr (symbol->safe (ast-symbol-value expr))))
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
