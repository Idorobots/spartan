#lang racket

;; Process scheduling tests.

(require "../testing.rkt")
(require "../../src/main.rkt")
(require "../../src/runtime/rt.rkt")
(require "../../src/runtime/scheduler.rkt")
(require "../../src/runtime/processes.rkt")
(require "../../src/runtime/continuations.rkt")
(require "../../src/runtime/closures.rkt")
(require "../../src/compiler/utils/utils.rkt")

(describe
 "scheduler"
 (it "Can step a process:"
     (assert (not (executable? (make-uproc 100 '() '() 0 'waiting))))
     (assert (executable? (make-uproc 100 (make-resumable (make-closure (make-env) id) '()) '() 0 'waiting)))
     (assert (uproc? (execute-step! (make-uproc 100 (make-resumable (make-closure (make-env) id) '()) '() 0 'waiting)))))

 (it "Can modify task list:"
     (let ((t (make-uproc 100 '() '() 0 'waiting)))
       (assert (begin (reset-tasks! (list t))
                      (next-task))
               t))
     (let* ((t1 (make-uproc 100 '() '() 1 'waiting))
            (t2 (make-uproc 100 '() '() 2 'waiting)))
       (assert (begin (reset-tasks! (list t1 t2))
                      (next-task))
               t1)
       (assert (begin (reset-tasks! (list t1 t2))
                      (dequeue-next-task!)
                      (next-task))
               t2)))

 (it "Can as easily resume stuff."
     (define test (make-closure '()
                                (lambda (_ a b cont)
                                  (make-resumable cont (equal? a b)))))
     (assert (resume
              (resume
               (resume
                (apply-closure
                 test
                 2
                 2
                 (make-closure
                  '()
                  (lambda (_ two)
                    (apply-closure
                     test
                     3
                     3
                     (make-closure
                      two
                      (lambda (two three)
                        (apply-closure
                         test
                         two
                         three
                         (make-closure
                          '()
                          (lambda (_ v) v))))))))))))))

 (it "Can run compiled code."
     (assert (run '23) 23)
     (assert (run '(= (* 3 2) (+ 3 3)))))

 (it "Runing stuff changes state."
     (let ((p (make-uproc 100 '() '() 0 'waiting)))
       (reset-tasks! (list p))
       (assert (uproc-state p) 'waiting)
       (dequeue-next-task!)
       (assert (uproc-state p) 'running))))
