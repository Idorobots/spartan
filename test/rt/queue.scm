;; Priority queue implementation.

;; Can enqueue/dequeue items.
(assert (queue-empty? (priority-queue <=)))

(assert (queue-min (queue-enqueue (priority-queue <=)
                                  23))
        23)

(assert (queue-empty? (queue-dequeue
                       (queue-enqueue
                        (priority-queue <=)
                        23))))

(assert (queue-min (queue-enqueue
                    (queue-enqueue
                     (priority-queue <=)
                     23)
                    5))
        5)

;; Priorities work:
(assert (queue-min (queue-enqueue
                    (queue-enqueue
                     (priority-queue <=)
                     5)
                    23))
        5)

(assert (queue-min (queue-dequeue
                    (queue-enqueue
                     (queue-enqueue
                      (priority-queue <=)
                      5)
                     23)))
        23)

(assert (queue-min (queue-enqueue
                    (queue-enqueue
                     (queue-enqueue
                      (priority-queue <=)
                      5)
                     23)
                    13))
        5)

;; Can handle complex things:
(assert (queue-min (queue-enqueue
                    (queue-enqueue
                     (priority-queue (lambda (a b)
                                       (<= (cadr a)
                                           (cadr b))))
                     (list 'foo 23))
                    (list 'bar 5)))
        '(bar 5))



