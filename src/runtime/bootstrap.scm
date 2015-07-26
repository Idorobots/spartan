;; The bootstrap code.

(load "runtime/rt.scm")

(define (cpsfy f)
  (lambda args
    (&yield-cont (last args)
                 (apply f
                        (take args
                              (- (length args) 1))))))

;; Built-in functions:
(define __cons (cpsfy cons))
(define __list (cpsfy list))

(define __MULT (cpsfy *))
(define __PLUS (cpsfy +))
(define ___ (cpsfy -))

(define __EQUAL (cpsfy =))
(define __LESS (cpsfy <))

;; Built-in values:
(define __nil nil)

;; Misc:
(define __task_info (cpsfy task-info))

