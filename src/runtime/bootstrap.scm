;; The bootstrap code.

(load "runtime/rt.scm")

(define (cpsfy f)
  (lambda args
    (&yield-cont (last args)
                 (apply f
                        (take args
                              (- (length args) 1))))))

(define __MULT (cpsfy *))
(define __PLUS (cpsfy +))
(define __EQUAL (cpsfy =))
