;; Syntax expansion module.

(load "compiler/qq.scm")
(load "compiler/ast.scm")

;; FIXME This should use a generic ast-walk.
(define (syntax-expand expr)
  (cond ((symbol? expr) (expand-symbol expr))
        ((number? expr) expr)
        ((string? expr) expr)
        ((vector? expr) expr)
        ((nil? expr) expr)
        ((char? expr) expr)
        ((quote? expr) expr)
        ('else (map syntax-expand expr))))

(define (expand-symbol expr)
  (let ((parts (map string->symbol
                    (string-split (symbol->string expr)
                                  "."))))
    (if (> (length parts) 1)
        (foldl (lambda (p a)
                 (make-app '&structure-ref (list a (make-quote p))))
               (car parts)
               (cdr parts))
        (car parts))))
