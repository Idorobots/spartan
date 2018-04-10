;; Target-safe variable renaming.

(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (mangle expr globals)
  (let ((rn (flip mangle globals)))
    (cond ((symbol? expr) (rename-symbol expr globals))
          ((number? expr) expr)
          ((string? expr) expr)
          ((vector? expr) expr)
          ((nil? expr) expr)
          ((char? expr) expr)
          ((quote? expr) expr)
          ((lambda? expr) (make-lambda (map rn (lambda-args expr))
                                       (make-do (map rn (lambda-body expr)))))
          ((do? expr) (make-do (map rn (do-statements expr))))
          ((if? expr) (make-if (rn (if-predicate expr))
                               (rn (if-then expr))
                               (rn (if-else expr))))
          ((letrec? expr) (make-letrec (map (partial map rn)
                                            (let-bindings expr))
                                       (make-do (map rn (let-body expr)))))
          ((application? expr) (make-app (rn (app-op expr))
                                         (map rn (app-args expr))))
          ;; These are required by broken letrec implementation.
          ((set!? expr) (make-set! (rn (set!-var expr))
                                   (rn (set!-val expr))))
          ;; These shouldn't be there after the phase.
          ((letcc? expr) (make-letcc (rn (let-bindings expr))
                                     (make-do (map rn (let-body expr)))))
          ((reset? expr) (make-reset (rn (reset-expr expr))))
          ((shift? expr) (make-shift (rn (shift-cont expr))
                                     (rn (shift-expr expr))))
          ((handle? expr) (make-handle (rn (handle-expr expr))
                                       (rn (handle-handler expr))))
          ((raise? expr) (make-raise (rn (raise-expr expr))))
          ;; These shouldn't be here.
          ((define? expr) (make-define-1 (rn (define-name expr))
                                         (rn (define-value expr))))
          ((let? expr) (make-let (map (partial map rn)
                                      (let-bindings expr))
                                 (make-do (map rn (let-body expr)))))
          ;; --
          ('else (error "Unexpected expression: " expr)))))

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
