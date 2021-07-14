#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

(require racket/future)

(provide optimize optimize-plain
         ;; FIXME For test access.
         estimate-performance)

(define (optimize-plain passes)
  (pass (schema "optimize") ;; NOTE Schema depends on the passes.
        (lambda (env)
          (let loop ((i +optimization-loops+)
                     (acc env)
                     (prev '()))
            (if (or (= i 0)
                    (equal? prev acc)) ;; FIXME This is needlessly slow.
                acc
                (loop (- i 1)
                      (foldl run-pass
                             acc
                             passes)
                      acc))))))

(struct scored (score env))

(define (optimize passes)
  (pass (schema "optimize") ;; NOTE Schema depends on the passes.
        (lambda (env)
          (let ((initial (score env)))
            (let loop ((i +optimization-loops+)
                       (runs (list initial))
                       (prev (scored +inf.0 '())))
              (let ((best-run (car runs)))
                (if (or (= i 0)
                        (not (score-better? best-run prev)))
                    (scored-env best-run)
                    (let* ((results (map (lambda (pass)
                                           (score
                                            (run-pass pass (scored-env best-run))))
                                         passes))
                           (sorted (sort (append results
                                                 runs)
                                         score-better?)))
                      (loop (- i 1)
                            sorted
                            best-run)))))))))

(define (score env)
  (scored (/ (+ (* +score-perf+ (estimate-performance (env-get env 'ast)))
                (* +score-size+ (ast-size (env-get env 'ast))))
             (+ +score-perf+ +score-size+))
          env))

(define (score-better? a b)
  (< (scored-score a)
     (scored-score b)))

(define +score-size+ 10.0)
(define +score-perf+ (- 100.0 +score-size+))
(define +optimization-loops+ 23)

(define (estimate-performance ast)
  (define cost-table
    (hasheq 'allocating-closure 100 ;; Allocating an env & closure, but not copying all the values there.
            'call 200               ;; Destructuring a closure, setting up the args, jumping to the code, does not include the body.
            'unknown-fun 1000       ;; Probably on the low side for recursive functions...
            'primop-call 50         ;; Seems like a fair value...
            'unknown-primop 100     ;; Again, probably on the low side...
            'memory-ref 10          ;; Should be pretty fast.
            'memory-set 20          ;; Probably a tad slower than a plain mem ref.
            'branch 10              ;; Should be fairly quick.
            'const-ref 1
            'noop 0))

  (define (cost-of* table key default-key)
    (if (hash-has-key? table key)
        (hash-ref table key)
        (hash-ref table default-key)))

  (define (cost-of table key)
    (cost-of* table key 'noop))

  (define (loop-let cost bindings body)
    (let* ((call-costs (map (lambda (binding)
                              (cons (ast-symbol-value (ast-binding-var binding))
                                    (loop cost (ast-lambda-body (ast-binding-val binding)))))
                            (filter (lambda (b)
                                      (ast-lambda? (ast-binding-val b)))
                                    bindings)))
           (updated-cost (foldl (lambda (b acc)
                                  (hash-set acc
                                            (car b)
                                            (cdr b)))
                                cost
                                call-costs)))
      (apply +
             (loop updated-cost body)
             (map (lambda (binding)
                    (loop cost binding))
                  bindings))))

  (define (loop-letrec cost bindings body)
    (let* ((funs (filter (lambda (b)
                           (ast-lambda? (ast-binding-val b)))
                         bindings))
           (updated-cost (foldl (lambda (b acc)
                                  (let ((var (ast-symbol-value (ast-binding-var b))))
                                    (hash-set acc
                                              var
                                              (loop (hash-set acc
                                                              var
                                                              ;; NOTE This is a lie, that produces better code.
                                                              (cost-of acc 'noop))
                                                    (ast-lambda-body (ast-binding-val b))))))
                                cost
                                funs)))
      (apply +
             (loop updated-cost body)
             (map (lambda (binding)
                    (loop cost binding))
                  bindings))))

  (define (loop-fun cost fun)
    (cost-of* cost
              (if (ast-symbol? fun)
                  (ast-symbol-value fun)
                  'unknown-fun)
              'unknown-fun))

  (define (loop cost expr)
    (case (ast-node-type expr)
      ((const symbol)
       (cost-of cost 'const-ref))
      ((if)
       (+ (cost-of cost 'branch)
          (loop cost (ast-if-condition expr))
          ;; NOTE Assumes worst case scenario.
          (max (loop cost (ast-if-then expr))
               (loop cost (ast-if-else expr)))))
      ((do)
       (apply +
              (map (lambda (expr)
                     (loop cost expr))
                   (ast-do-exprs expr))))
      ((lambda)
       ;; NOTE This is just the closure creation. The body complexity is taken into account when processing binders.
       (+ (cost-of cost 'allocating-closure)
          (* (length (ast-node-free-vars expr))
             (+ (cost-of cost 'memory-ref)
                (cost-of cost 'memory-set)))))
      ((binding)
       (+ (cost-of cost 'memory-set)
          (loop cost (ast-binding-val expr))))
      ((let)
       (loop-let cost
                 (ast-let-bindings expr)
                 (ast-let-body expr)))
      ((letrec)
       (loop-letrec cost
                    (ast-letrec-bindings expr)
                    (ast-letrec-body expr)))
      ((fix)
       (loop-letrec cost
                    (ast-fix-bindings expr)
                    (ast-fix-body expr)))
      ((app)
       (apply +
              (cost-of cost 'call)
              (loop-fun cost (ast-app-op expr))
              (loop cost (ast-app-op expr))
              (map (lambda (expr)
                     (loop cost expr))
                   (ast-app-args expr))))
      ((primop-app)
       (apply +
              (cost-of cost 'primop-call)
              (cost-of* cost (ast-primop-app-op expr) 'unknown-primop)
              (map (lambda (expr)
                     (loop cost expr))
                   (ast-primop-app-args expr))))
      ;; The rest are compiled away, so they are essentially zero cost abstractions.
      (else
       (cost-of cost 'noop))))

  (loop cost-table ast))
