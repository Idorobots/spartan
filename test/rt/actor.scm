;; Actor model tests.

;; Can sleep for a time.
(define (near-enough? value expected delta)
  (and (>= value (- expected delta))
       (<= value (+ expected delta))))

(let ((p (uproc 100
                (&yield-cont (lambda (v)
                               (__sleep v id))
                             23)
                0
                'waiting)))
  (reset-tasks! (list p))
  (execute!)
  (assert (near-enough? (uproc-rtime p) 23 1))
  (assert (near-enough? (uproc-vtime p) 2300 100)))

;; Can retrieve own pid.
(gensym-reset!)
(assert (run '(self)) '__pid1)

;; TODO Can retrieve current node.

;; Can send a message.
(let ((p (uproc 100
                (&yield-cont (lambda (v)
                               (__self (lambda (__value4)
                                         (__send __value4
                                                 v
                                                 (lambda (__value3)
                                                   __value3)))))
                             'msg)
                0
                'waiting)))
  (reset-tasks! (list p))
  (execute!)
  (assert (not (empty? (uproc-msg-queue p))))
  (assert (equal? (first (uproc-msg-queue p)) 'msg)))

(let ((p (uproc 100
                (&yield-cont (lambda (v)
                               (__self
                                (lambda (__value7)
                                  (__send
                                   __value7
                                   v
                                   (lambda (__value6)
                                     (__send __value6
                                             v
                                             (lambda (__value5)
                                               __value5)))))))
                             'msg)
                0
                'waiting)))
  (reset-tasks! (list p))
  (execute!)
  (assert (equal? (length (uproc-msg-queue p)) 2))
  (assert (equal? (first (uproc-msg-queue p)) 'msg)))

;; Can't receive when there are no messages.
(let ((p (uproc 100
                (&yield-cont (lambda (_)
                               (__recv id))
                             nil)
                0
                'waiting)))
  (reset-tasks! (list p))
  (execute!)
  (assert (uproc-state p) 'waiting-4-msg))

;; Can receive a message.
(assert (run '(do (send (self) 'msg)
                  (recv)))
        'msg)

;; Messages are received in the correct order.
(assert (run '(do (send (self) 1)
                  (send (self) 2)
                (recv)))
        1)

;; Can spawn a process.
(gensym-reset!)
(assert (run '(spawn (lambda ()
                       (* 1 (+ 2 3)))))
        '__pid6)
