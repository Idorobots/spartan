#lang racket

(require "../env.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../utils/pmap.rkt")

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

(define +optimization-loops+ 23)

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
                    (let* ((results (pmap (lambda (pass)
                                           (score
                                            (run-pass pass (scored-env best-run))))
                                         passes))
                           (sorted (sort (append results
                                                 runs)
                                         score-better?)))
                      (loop (- i 1)
                            sorted
                            best-run)))))))))

(define +score-size+ 10.0)
(define +score-perf+ (- 100.0 +score-size+))

(define (score-table . args)
  (apply hasheq args))

(define +perf-cost+
  (score-table 'allocating-closure 500 ;; Allocating an env & closure, but not copying all the values there.
               'call 500               ;; Destructuring a closure, setting up the args, jumping to the code, does not include the body.
               'unknown-fun 5000       ;; Probably on the low side for recursive functions...
               'primop-call 100        ;; Seems like a fair value...
               'unknown-primop 500     ;; Again, probably on the low side...
               'memory-ref 10          ;; Should be pretty fast.
               'memory-set 20          ;; Probably a tad slower than a plain mem ref.
               'branch 10              ;; Should be fairly quick.
               'const-ref 1
               'noop 0))

(struct scored (score env))

(define (score env)
  (scored (/ (+ (* +score-perf+ (estimate-performance (env-get env 'ast) +perf-cost+))
                (* +score-size+ (ast-size (env-get env 'ast))))
             (+ +score-perf+ +score-size+))
          env))

(define (score-better? a b)
  (< (scored-score a)
     (scored-score b)))

(define (score-of* table key default-key)
  (if (hash-has-key? table key)
      (hash-ref table key)
      (hash-ref table default-key)))

(define (score-of table key)
  (score-of* table key 'noop))

(define (set-score table key value)
  (hash-set table key value))

(define (estimate-performance ast cost-table)
  (define (loop-let cost bindings body)
    (let* ((call-costs (map (lambda (binding)
                              (cons (ast-symbol-value (ast-binding-var binding))
                                    (loop cost (ast-lambda-body (ast-binding-val binding)))))
                            (filter (lambda (b)
                                      (ast-lambda? (ast-binding-val b)))
                                    bindings)))
           (updated-cost (foldl (lambda (b acc)
                                  (set-score acc
                                             (car b)
                                             (cdr b)))
                                cost
                                call-costs)))
      (apply +
             (loop updated-cost body)
             (map (lambda (binding)
                    (loop cost binding))
                  bindings))))

  (define (loop-rec cost bindings body)
    (let* ((funs (filter (lambda (b)
                           (ast-lambda? (ast-binding-val b)))
                         bindings))
           (updated-cost (foldl (lambda (b acc)
                                  (let ((var (ast-symbol-value (ast-binding-var b))))
                                    (set-score acc
                                               var
                                               (loop (set-score acc
                                                                var
                                                                ;; NOTE This is a lie, that produces better code.
                                                                (score-of acc 'noop))
                                                     (ast-lambda-body (ast-binding-val b))))))
                                cost
                                funs)))
      (apply +
             (loop updated-cost body)
             (map (lambda (binding)
                    (loop cost binding))
                  bindings))))

  (define (loop cost expr)
    (case (ast-node-type expr)
      ((const symbol)
       (score-of cost 'const-ref))
      ((if)
       (+ (score-of cost 'branch)
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
       (+ (score-of cost 'allocating-closure)
          (* (length (ast-node-free-vars expr))
             (+ (score-of cost 'memory-ref)
                (score-of cost 'memory-set)))))
      ((binding)
       (+ (score-of cost 'memory-set)
          (loop cost (ast-binding-val expr))))
      ((let)
       (loop-let cost
                 (ast-let-bindings expr)
                 (ast-let-body expr)))
      ((letrec)
       (loop-rec cost
                 (ast-letrec-bindings expr)
                 (ast-letrec-body expr)))
      ((fix)
       (loop-rec cost
                 (ast-fix-bindings expr)
                 (ast-fix-body expr)))
      ((app)
       (let ((op (ast-app-op expr)))
         (apply +
                (score-of cost 'call)
                (score-of* cost
                           (if (ast-symbol? op)
                               (ast-symbol-value op)
                               'unknown-fun)
                           'unknown-fun)
                (loop cost op)
                (map (lambda (expr)
                       (loop cost expr))
                     (ast-app-args expr)))))
      ((primop-app)
       (apply +
              (score-of cost 'primop-call)
              (score-of* cost (ast-primop-app-op expr) 'unknown-primop)
              (map (lambda (expr)
                     (loop cost expr))
                   (ast-primop-app-args expr))))
      ;; The rest are compiled away, so they are essentially zero cost abstractions.
      (else
       (score-of cost 'noop))))

  (loop cost-table ast))
