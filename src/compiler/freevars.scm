;; Free vars computation

(load "compiler/ast.scm")
(load "compiler/utils.scm")

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

;; FIXME This ought to be a separate AST annotation phase.
;; FIXME Rewrite in terms of ast/walk.
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
        ((let? expr) (set-union (set-sum (map (compose free-vars cadr)
                                              (let-bindings expr)))
                                (set-difference (free-vars (let-body expr))
                                                (set-sum (map (compose free-vars car)
                                                              (let-bindings expr))))))
        ((or (letrec? expr)
             (fix? expr)) (set-difference (set-union (set-sum (map (compose free-vars cadr)
                                                                   (let-bindings expr)))
                                                     (free-vars (let-body expr)))
                                          (set-sum (map (compose free-vars car)
                                                        (let-bindings expr)))))
        ((application? expr) (set-union (free-vars (app-op expr))
                                        (foldl set-union
                                               (set)
                                               (map free-vars
                                                    (app-args expr)))))
        ;; These are required by broken letrec implementation.
        ((set!? expr) (free-vars (set!-val expr)))
        ;; --
        ('else (error "Unexpected expression:" expr))))
