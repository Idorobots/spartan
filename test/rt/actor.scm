;; Actor model tests.

(define (near-enough? value expected delta)
  (and (>= value (- expected delta))
       (<= value (+ expected delta))))

(describe
 "Actor Model"
 (it "Can sleep for a time."
     (let ((p (uproc 100
                     (&yield-cont (closurize
                                   (lambda (v)
                                     (&apply __sleep v (closurize id))))
                                  23)
                     nil
                     0
                     'waiting)))
       (reset-tasks! (list p))
       (execute!)
       (assert (near-enough? (uproc-rtime p) 23 1))
       (assert (near-enough? (uproc-vtime p) 2300 100))))

 (it "Can retrieve own pid."
     (gensym-reset!)
     (assert (run '(self)) 'pid3))

 (ignore "Can retrieve current node."
          ;; There is no notion of a node yet.
          (error "Actually get this test to work!"))

 (it "Can send a message."
     (let ((p (uproc 100
                     (&yield-cont (closurize
                                   (lambda (v)
                                     (&apply __self (closurize
                                                     (lambda (__value4)
                                                       (&apply __send
                                                               __value4
                                                               v
                                                               (closurize
                                                                (lambda (__value3)
                                                                  __value3))))))))
                                  'msg)
                     nil
                     0
                     'waiting)))
       (reset-tasks! (list p))
       (execute!)
       (assert (not (empty? (uproc-msg-queue p))))
       (assert (equal? (first (uproc-msg-queue p)) 'msg)))
     (let ((p (uproc 100
                     (&yield-cont (closurize
                                   (lambda (v)
                                     (&apply __self
                                             (closurize
                                              (lambda (__value7)
                                                (&apply __send
                                                        __value7
                                                        v
                                                        (closurize
                                                         (lambda (__value6)
                                                           (&apply __send
                                                                   __value6
                                                                   v
                                                                   (closurize
                                                                    (lambda (__value5)
                                                                      __value5)))))))))))
                                  'msg)
                     nil
                     0
                     'waiting)))
       (reset-tasks! (list p))
       (execute!)
       (assert (equal? (length (uproc-msg-queue p)) 2))
       (assert (equal? (first (uproc-msg-queue p)) 'msg))))

 (it "Can't receive when there are no messages."
     (let ((p (uproc 100
                     (&yield-cont (closurize
                                   (lambda (_)
                                     (&apply __recv (closurize id))))
                                  nil)
                     nil
                     0
                     'waiting)))
       (reset-tasks! (list p))
       (execute!)
       (assert (uproc-state p) 'waiting-4-msg)))

 (it "Can receive a message."
      (assert (run '(do (send (self) 'msg)
                        (recv)))
              'msg))

 (it "Messages are received in the correct order."
     (assert (run '(do (send (self) 1)
                       (send (self) 2)
                     (recv)))
             1))

 (it "Can spawn a process."
     (gensym-reset!)
     (assert (run '(spawn (lambda ()
                            (* 1 (+ 2 3)))))
             'pid12)))
