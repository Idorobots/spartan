;; Continuation Passing Converter
;; Assumes macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (cpc expr kont)
  (cond ((symbol? expr) (kont expr))
        ((number? expr) (kont expr))
        ((string? expr) (kont expr))
        ((vector? expr) (kont expr))
        ((nil? expr) (kont expr))
        ((char? expr) (kont expr))
        ((quote? expr) (kont expr))
        ((lambda? expr) (cpc-lambda expr kont))
        ((do? expr) (cpc-do expr kont))
        ((if? expr) (cpc-if expr kont))
        ((letrec? expr) (cpc-let make-letrec expr kont))
        ((and (application? expr)
              (not (primop-application? expr))) (cpc-app expr kont))
        ;; FIXME This is required due to a broken modules implementation.
        ((primop-application? expr) (kont (make-app (app-op expr)
                                                    (map (flip cpc (make-identity-continuation))
                                                         (app-args expr)))))
        ;; These shouldn't be there after the phase.
        ((letcc? expr) (cpc-letcc expr kont))
        ((reset? expr) (cpc-reset expr kont))
        ((shift? expr) (cpc-shift expr kont))
        ((handle? expr) (cpc-handle expr kont))
        ((raise? expr) (cpc-raise expr kont))
        ;; These shouldn't be here.
        ((define? expr) (cpc-define expr kont))
        ((let? expr) (cpc-let make-let expr kont))
        ;; --
        ('else (error "Unexpected expression: " expr))))

(define (make-identity-continuation)
  id)

(define (primop-application? expr)
  (and (application? expr)
       (member (app-op expr) (make-global-environment))))

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
           (cpc-sequence (lambda-body expr)
                         (lambda (sts)
                           (returning-last sts
                                           (lambda (s)
                                             (make-yield (make-app-1 ct s))))))))))

(define (cpc-define expr kont)
  (kont (make-define-1 (define-name expr)
                       (cpc (define-value expr)
                            ;; FIXME Shouldn't this be the other definitions?
                            (make-identity-continuation)))))

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

(define (cpc-let builder expr kont)
  (let* ((bindings (let-bindings expr))
         (names (map car bindings))
         (values (map cadr bindings)))
    (builder (map (lambda (v)
                    (list v (make-quote '())))
                  names)
             (cpc-sequence values
                           (lambda (sts)
                             (make-do (append (map make-set! names sts)
                                              (list (cpc-sequence (let-body expr)
                                                                  (lambda (sts)
                                                                    (returning-last sts kont)))))))))))

(define (cpc-app expr kont)
  (let ((value (gensym 'value)))
    (cpc (app-op expr)
         (lambda (op)
           (cpc-sequence (app-args expr)
                         (lambda (args)
                           (make-app op
                                     (append args (list (make-lambda-1 value
                                                                       (kont value)))))))))))

(define (cpc-letcc expr kont)
  (let ((cc (let-bindings expr))
        (v1 (gensym 'value))
        (v2 (gensym 'value))
        (ct (gensym 'cont)))
    (make-let-1 ct (make-lambda-1 v1 (kont v1))
                (make-let-1 cc (make-lambda-2 v2 (gensym 'ignored)
                                              (make-yield (make-app-1 ct v2)))
                            (cpc-sequence (let-body expr)
                                          (lambda (sts)
                                            (returning-last sts
                                                            (lambda (v)
                                                              (make-yield (make-app-1 ct v))))))))))

(define (cpc-reset expr kont)
  (let ((value (gensym 'value)))
    (make-app-1 (make-lambda-1 value (kont value))
                ;; NOTE This is purely static. Might need some continuation stack
                ;; NOTE maintenance in order to implement push-prompt etc.
                (cpc (reset-expr expr)
                     (make-identity-continuation)))))

(define (cpc-shift expr kont)
  (let ((name (shift-cont expr))
        (ct (gensym 'cont))
        (value (gensym 'value)))
    (make-let-1 name
                (make-lambda-2 value ct
                               (make-app-1 ct (kont value)))
                (cpc (shift-expr expr)
                     (make-identity-continuation)))))

(define (with-handler h next)
  (make-do (list (make-app-1 '&set-error-handler! h)
                 next)))

(define (cpc-handle expr kont)
  (let ((ct (gensym 'cont))
        (handler (gensym 'handler))
        (value (gensym 'value))
        (restart (gensym 'restart))
        (error (gensym 'error)))
    (make-let (list (list handler (make-app '&error-handler nil))
                    (list ct (make-lambda-1 value (kont value))))
              (cpc (handle-handler expr)
                   (lambda (h)
                     (let ((wrap (make-lambda-2 error restart
                                                (with-handler handler
                                                              (make-app h
                                                                        (list error
                                                                              restart
                                                                              ct))))))
                       (with-handler wrap
                                     (cpc (handle-expr expr)
                                          (lambda (v)
                                            (with-handler handler
                                                          (make-yield (make-app-1 ct v))))))))))))

(define (cpc-raise expr kont)
  (let ((value (gensym 'value))
        (ignored (gensym 'ignored))
        (h (gensym 'handler)))
    (make-let-1 h
                (make-app '&error-handler nil)
                (cpc (raise-expr expr)
                     (lambda (v)
                       (make-app h
                                 (list v (make-lambda-2 value ignored
                                                        (with-handler h
                                                                      (kont value))))))))))
