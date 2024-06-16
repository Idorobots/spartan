#lang racket

;; Actor model tests.

(require "../testing.rkt")
(require "../../src/main.rkt")
(require "../../src/runtime/rt.rkt")
(require "../../src/compiler/utils/gensym.rkt")
(require "../../src/compiler/utils/utils.rkt")

(define (near-enough? value expected delta)
  (and (>= value (- expected delta))
       (<= value (+ expected delta))))

(define __core (bootstrap-core-once!))
(define __self (intern-instrument '__self))
(define __send (intern-instrument '__send))
(define __recv (intern-instrument '__recv))
(define __sleep (intern-instrument '__sleep))

(describe
 "Actor Model"
 (it "Can sleep for a time."
     (let ((p (make-uproc 100
                          (make-resumable
                           (closurize
                            (lambda (v)
                              (apply-closure __sleep v (closurize id))))
                           23)
                          '()
                          0
                          'waiting)))
       (reset-tasks! (list p))
       (execute!)
       (assert (near-enough? (uproc-rtime p) 23 1))
       (assert (near-enough? (uproc-vtime p) 2300 100))))

 (it "Can retrieve own pid."
     (gensym-reset!)
     (assert (run '(self)) 'pid2))

 (ignore "Can retrieve current node."
         ;; There is no notion of a node yet.
         (error "Actually get this test to work!"))

 (it "Can send a message."
     (let ((p (make-uproc 100
                          (make-resumable
                           (closurize
                            (lambda (v)
                              (apply-closure __self (closurize
                                                     (lambda (__value4)
                                                       (apply-closure __send
                                                                      __value4
                                                                      v
                                                                      (closurize
                                                                       (lambda (__value3)
                                                                         __value3))))))))
                           'msg)
                          '()
                          0
                          'waiting)))
       (reset-tasks! (list p))
       (execute!)
       (assert (not (empty? (uproc-msg-queue p))))
       (assert (equal? (first (uproc-msg-queue p)) 'msg)))
     (let ((p (make-uproc 100
                          (make-resumable
                           (closurize
                            (lambda (v)
                              (apply-closure __self
                                             (closurize
                                              (lambda (__value7)
                                                (apply-closure __send
                                                               __value7
                                                               v
                                                               (closurize
                                                                (lambda (__value6)
                                                                  (apply-closure __send
                                                                                 __value6
                                                                                 v
                                                                                 (closurize
                                                                                  (lambda (__value5)
                                                                                    __value5)))))))))))
                           'msg)
                          '()
                          0
                          'waiting)))
       (reset-tasks! (list p))
       (execute!)
       (assert (equal? (length (uproc-msg-queue p)) 2))
       (assert (equal? (first (uproc-msg-queue p)) 'msg))))

 (it "Can't receive when there are no messages."
     (let ((p (make-uproc 100
                          (make-resumable
                           (closurize
                            (lambda (_)
                              (apply-closure __recv (closurize id))))
                           '())
                          '()
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
             'pid7)))
