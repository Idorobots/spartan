;; Syntax expansion module.

(load "compiler/qq.scm")
(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (syntax-expand expr)
  (walk (lambda (expr)
          (cond ((lambda? expr)
                 (make-lambda (lambda-args expr)
                              (make-do (lambda-body* expr))))

                ((value-define? expr)
                 expr)

                ((define? expr)
                 (make-val-define (define-name expr)
                              (make-lambda (define-args expr)
                                           (make-do (define-body* expr)))))

                ((let? expr)
                 (make-let (let-bindings expr)
                           (make-do (let-body* expr))))

                ((letrec? expr)
                 (make-letrec (let-bindings expr)
                              (make-do (let-body* expr))))

                ((letcc? expr)
                 (make-letcc (let-bindings expr)
                             (make-do (let-body* expr))))

                ('else
                 expr)))
        (lambda (expr)
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
