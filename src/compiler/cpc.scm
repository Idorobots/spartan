;; Continuation Passing Converter
;; Assumes macro-expanded code.

(load "compiler/rename.scm")
(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (cpc expr kont)
  (cond ((simple-expression? expr) (kont (cpc-simple expr)))
        ((define? expr) (cpc-define expr kont))
        ((do? expr) (cpc-do expr kont))
        ((if? expr) (cpc-if expr kont))
        ((letcc? expr) (cpc-letcc expr kont))
        ((letrec? expr) (cpc-letrec expr kont))
        ((reset? expr) (cpc-reset expr kont))
        ((shift? expr) (cpc-shift expr kont))
        ((application? expr) (cpc-app expr kont))))

(define (cpc-simple expr)
  (cond ((symbol? expr) (cpc-symbol expr))
        ((number? expr) (cpc-number expr))
        ((string? expr) (cpc-string expr))
        ((vector? expr) (cpc-vector expr))
        ((nil? expr) (cpc-nil expr))
        ((char? expr) (cpc-character expr))
        ((quote? expr) (cpc-quote expr))
        ((lambda? expr) (cpc-lambda expr))))

(define (cpc-symbol expr)
  (symbol->safe expr))

(define (cpc-number expr)
  expr)

(define (cpc-string expr)
  expr)

(define (cpc-vector expr)
  expr)

(define (cpc-nil expr)
  expr)

(define (cpc-character expr)
  expr)

(define (cpc-quote expr)
  expr)

(define (cpc-lambda expr)
  (let ((ct (gensym 'cont))
        (args (map symbol->safe (lambda-args expr))))
    (make-lambda
     (append args (list ct))
     (cpc-sequence (lambda-body expr)
                   (lambda (sts)
                     (returning-last sts
                                     (lambda (s)
                                       (make-yield (make-app-1 ct s)))))))))

(define (cpc-define expr kont)
  (kont (make-define-1 (symbol->safe (define-name expr))
                       (cpc (define-value expr)
                            ;; FIXME Shouldn't this be the other definitions?
                            (make-identity-continuation)))))

(define (cpc-do expr kont)
  (cpc-sequence (do-statements expr)
                (lambda (sts)
                  (returning-last sts kont))))

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

(define (cpc-letcc expr kont)
  (let* ((cc (symbol->safe (let-bindings expr)))
         (v1 (gensym 'value))
         (v2 (gensym 'value))
         (ct (gensym 'cont)))
    (make-let (list (list ct (make-lambda-1 v1 (kont v1)))
                    (list cc (make-lambda-2 v2 (gensym 'ignored)
                                            (make-yield (make-app-1 ct v2)))))
              (cpc-sequence (let-body expr)
                            (lambda (sts)
                              (returning-last sts ct))))))

(define (cpc-letrec expr kont)
  (let* ((bindings (let-bindings expr))
         (names (map (lambda (b) (symbol->safe (car b))) bindings))
         (values (map cadr bindings)))
    (make-let (map (lambda (v)
                     (list v (make-quote '())))
                   names)
              (cpc-sequence values
                            (lambda (sts)
                              (make-do (append (map make-set! names sts)
                                        (list (cpc-sequence (let-body expr)
                                                             (lambda (sts)
                                                               (returning-last sts kont)))))))))))

(define (cpc-reset expr kont)
  (let* ((value (gensym 'value)))
    (make-app-1 (make-lambda-1 value (kont value))
                ;; NOTE This is purely static. Might need some continuation stack
                ;; NOTE maintenance in order to implement push-prompt etc.
                (cpc (reset-expr expr)
                     (make-identity-continuation)))))

(define (cpc-shift expr kont)
  (let* ((name (symbol->safe (shift-cont expr)))
         (ct (gensym 'cont))
         (value (gensym 'value)))
    (make-let-1 name
                (make-lambda-2 value ct
                               (make-app-1 ct (kont value)))
                (cpc (shift-expr expr)
                     (make-identity-continuation)))))

(define (cpc-app expr kont)
  (let* ((value (gensym 'value))
         (cont (make-lambda-1 value (kont value))))
    (cpc (app-op expr)
         (lambda (op)
           (cpc-sequence (app-args expr)
                         (lambda (args)
                           (make-app op (append args (list cont)))))))))

(define (make-yield expr)
  (cons '&yield-cont expr))
