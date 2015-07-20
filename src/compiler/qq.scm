;; A crude quasiquote type deal.

(load "compiler/utils.scm")

(define (quasiquote-expand expr)
  (if (tagged-list? 'unquote-splicing expr)
      (raise '(invalid-splicing-context expr))
      (quasiquote-expand-1 expr)))

(define (quasiquote-expand-1 expr)
  (cond ((tagged-list? 'quasiquote expr) expr)
        ((tagged-list? 'unquote expr) (cadr expr))
        ((tagged-list? 'unquote-splicing expr) (cadr expr))
        ((pair? expr) (quasiquote-expand-form expr))
        ;; TODO Vectors etc.
        ('else (cons 'quote (cons expr nil)))))

(define (quasiquote-expand-form exprs)
  (if (empty? exprs)
      ''()
      ((if (tagged-list? 'unquote-splicing (car exprs))
           make-concat
           make-cons)
       (quasiquote-expand-1 (car exprs))
       (quasiquote-expand-form (cdr exprs)))))

(define (concat a b)
  (if (empty? a)
      b
      (cons (car a) (concat (cdr a) b))))

(define (make-concat a b)
  (cons 'concat (cons a (cons b '()))))

(define (make-cons a b)
  (cons 'cons (cons a (cons b '()))))