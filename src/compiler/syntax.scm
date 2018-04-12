;; Syntax expansion module.

(load "compiler/qq.scm")
(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (syntax-expand expr)
  (walk (lambda (expr)
          (if (symbol? expr)
              (expand-symbol expr)
              expr))
        expr))

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
