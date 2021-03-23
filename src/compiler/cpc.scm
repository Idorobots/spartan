;; Continuation Passing Converter
;; Assumes syntax & macro-expanded code.

(load "compiler/utils/gensym.scm")
(load "compiler/utils/utils.scm")

(load "compiler/ast.scm")

(define (cpc expr kont)
  (cond ((symbol? expr) (kont expr))
        ((number? expr) (kont expr))
        ((string? expr) (kont expr))
        ((vector? expr) (kont expr))
        ((nil? expr) (kont expr))
        ((char? expr) (kont expr))
        ((quote? expr) (kont expr))
        ((if? expr) (cpc-if expr kont))
        ((do? expr) (cpc-do expr kont))
        ((let? expr) (cpc-let expr kont))
        ((lambda? expr) (cpc-lambda expr kont))
        ((fix? expr) (cpc-fix expr kont))
        ((application? expr) (cpc-app expr kont))
        ;; --
        ('else (error "Unexpected expression: " expr))))

(define (make-identity-continuation)
  id)

(define (make-yield expr)
  (cons '&yield-cont expr))

(define (cpc-sequence exprs kont)
  (if (empty? exprs)
      (kont nil)
      (cpc (first-statement exprs)
           (lambda (fst)
             (cpc-sequence (rest-statements exprs)
                           (lambda (rst)
                             (kont (cons fst rst))))))))

(define (returning-last statements kont)
  (make-do (returning-last* statements kont)))

(define (returning-last* statements kont)
  (if (empty? (rest-statements statements))
      (list (kont (first-statement statements)))
      (cons (first-statement statements)
            (returning-last* (rest-statements statements)
                             kont))))

(define (cpc-lambda expr kont)
  (let ((ct (gensym 'cont)))
    (kont (make-lambda
           (append (lambda-args expr) (list ct))
           (cpc (lambda-body expr)
                (lambda (s)
                  (make-yield (make-app-1 ct s))))))))

(define (cpc-do expr kont)
  (cpc-sequence (do-statements expr)
                (lambda (sts)
                  (returning-last sts kont))))

(define (cpc-if expr kont)
  (let* ((ct (gensym 'cont))
         (rest (lambda (v) (make-yield (make-app-1 ct v))))
         (value (gensym 'value)))
    (cpc (if-predicate expr)
         (lambda (condition)
           (make-let-1 ct (make-lambda-1 value (kont value))
                       (make-if condition
                                (cpc (if-then expr) rest)
                                (cpc (if-else expr) rest)))))))

(define (cpc-fix expr kont)
  (make-fix (map (lambda (b)
                   (list (car b)
                         (cpc (cadr b)
                              (make-identity-continuation))))
                 (let-bindings expr))
            (cpc (fix-body expr)
                 kont)))

(define (binding-steps names values finally)
  (if (empty? names)
      finally
      (cpc (car values)
           (lambda (v)
             (make-let-1 (car names) v
                         (binding-steps (cdr names)
                                        (cdr values)
                                        finally))))))

(define (cpc-let expr kont)
  (let ((bindings (let-bindings expr)))
    (binding-steps (map car bindings)
                   (map cadr bindings)
                   (cpc (let-body expr)
                        kont))))

(define (cpc-app expr kont)
  (if (primop-application? expr)
      (kont (make-app (app-op expr)
                      (map (flip cpc (make-identity-continuation))
                           (app-args expr))))
      (let ((value (gensym 'value)))
        (cpc (app-op expr)
             (lambda (op)
               (cpc-sequence (app-args expr)
                             (lambda (args)
                               (make-app op
                                         (append args (list (make-lambda-1 value
                                                                           (kont value))))))))))))
