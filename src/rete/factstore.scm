;; Fact store for storing facts.

(load-once "rete/utils.scm")

;; State:

(define *facts* (ref null))

(define (reset-facts!)
  (assign! *facts* null))

;; Fact store operations:

(define (add-fact! fact)
  (unless (fact-asserted? fact)
    (assign! *facts* (cons fact (deref *facts*)))))

(define (remove-fact! fact)
  (assign! *facts*
           (filter (partial not-equal? fact)
                   (deref *facts*))))

(define (fact-reductor r f v)
  (r f v (deref *facts*)))

(define foldl-facts (partial fact-reductor foldl))
(define foldr-facts (partial fact-reductor foldr))

(define (map-facts f)
  (map f (deref *facts*)))

;; Utils:

(define (fact-asserted? fact)
  (foldl-facts (lambda (f acc)
                 (or acc (equal? fact f)))
               #f))

(define (list-facts)
  (map-facts id))
