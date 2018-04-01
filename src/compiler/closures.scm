;; Closure conversion.
;; Assumes macro-expanded code.

(load "compiler/ast.scm")
(load "compiler/utils.scm")

(define (closure-convert expr globals)
  (let ((cc (flip closure-convert globals)))
    (cond ((lambda? expr) (cc-lambda (make-lambda (lambda-args expr)
                                                  (cc (car (lambda-body expr))))
                                     globals))
          ((simple-expression? expr) expr)
          ((define? expr) (make-define-1 (define-name expr)
                                         (cc (define-value expr))))
          ((do? expr) (make-do (map cc (do-statements expr))))
          ((if? expr) (make-if (cc (if-predicate expr))
                               (cc (if-then expr))
                               (cc (if-else expr))))
          ((let? expr) expr) ;; TODO
          ((letcc? expr) (make-letcc (let-bindings expr)
                                     (cc (car (let-body expr)))))
          ((letrec? expr) expr) ;; TODO
          ((reset? expr) (make-reset (cc (reset-expr expr))))
          ((shift? expr) (make-shift (shift-cont expr)
                                     (cc (shift-expr expr))))
          ((handle? expr) (make-handle (cc (handle-expr expr))
                                       (cc (handle-handler expr))))
          ((raise? expr) (make-raise (cc (raise-expr expr))))
          ((application? expr) (make-app '&apply
                                         (cons (cc (app-op expr))
                                               (map cc (app-args expr))))))))

(define (flip f x)
  (lambda (y)
    (f y x)))

(define (cc-lambda expr globals)
  (let ((env (gensym 'env))
        (args (lambda-args expr))
        (body (car (lambda-body expr)))
        (free (set-difference (free-vars expr)
                              globals)))
    (make-app '&make-closure
              (list (make-app '&make-env free)
                    (make-lambda (cons env args)
                                 (substitute (map (lambda (var)
                                                    (cons var
                                                          (make-app '&env-ref
                                                                    (list env
                                                                          (offset var free)))))
                                                  free)
                                             body))))))

(define (offset needle haystack)
  (- (length haystack)
     (length (member needle haystack))))

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))

(define (set . args)
  (sort args symbol<?))

(define (set-difference as bs)
  (filter (lambda (a)
            (not (member a bs)))
          as))

(define (set-union as bs)
  (sort (append as (set-difference bs as))
        symbol<?))

(define (set-sum sets)
  (foldl set-union (set) sets))

(define (free-vars expr)
  (cond ((symbol? expr) (set expr))
        ((number? expr) (set))
        ((string? expr) (set))
        ((vector? expr) (set))
        ((nil? expr) (set))
        ((char? expr) (set))
        ((quote? expr) (set))
        ((lambda? expr) (set-difference (free-vars (lambda-body expr))
                                        (apply set (lambda-args expr))))
        ((define? expr) (free-vars (define-value expr)))
        ((do? expr) (set-sum (map free-vars
                                  (do-statements expr))))
        ((if? expr) (set-sum (list (free-vars (if-predicate expr))
                                   (free-vars (if-then expr))
                                   (free-vars (if-else expr)))))
        ((letcc? expr) (set-difference (free-vars (let-body expr))
                                       (set (let-bindings expr))))
        ((letrec? expr) (set-difference (set-union (set-sum (map (compose free-vars cadr)
                                                                 (let-bindings expr)))
                                                   (free-vars (let-body expr)))
                                        (set-sum (map (compose free-vars car)
                                                      (let-bindings expr)))))
        ((reset? expr) (free-vars (reset-expr expr)))
        ((shift? expr) (set-difference (free-vars (shift-expr expr))
                                       (set (shift-cont expr))))
        ((handle? expr) (set-union (free-vars (handle-expr expr))
                                   (free-vars (handle-handler expr))))
        ((raise? expr) (free-vars (raise-expr expr)))
        ((application? expr) (set-union (free-vars (app-op expr))
                                        (foldl set-union
                                               (set)
                                               (map free-vars
                                                    (app-args expr)))))))

(define (substitute subs expr)
  (cond ((symbol? expr) (subs-symbol subs expr))
        ((number? expr) expr)
        ((string? expr) expr)
        ((vector? expr) expr)
        ((nil? expr) expr)
        ((char? expr) expr)
        ((quote? expr) expr)
        ('else (map (partial substitute subs) expr))))

(define (subs-symbol subs symbol)
  (let ((a (assoc symbol subs)))
    (if a
        (cdr a)
        symbol)))
