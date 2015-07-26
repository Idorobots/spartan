;; Actor model tests.

;; Can sleep for a time.
(define (near-enough? value expected delta)
  (and (>= value (- expected delta))
       (<= value (+ expected delta))))

(let ((p (uproc 100
                (&yield-cont (lambda (v)
                               (__sleep v id))
                             23)
                0)))
  (reset-tasks! (list p))
  (execute!)
  (assert (near-enough? (uproc-rtime p) 23 1))
  (assert (near-enough? (uproc-vtime p) 2300 100)))

;; TODO Can retrieve own pid.

;; TODO Can retrieve current node.

;; TODO Can send a message.

;; TODO Can receive a message.

;; TODO Can spawn a process.
