;; The bootstrap code.

(load "runtime/rt.scm")

;; Built-in values:
(define __nil nil)

;; Built-in functions:
(define (cpsfy f)
  (lambda args
    (&yield-cont (last args)
                 (apply f
                        (take args
                              (- (length args) 1))))))

(define __cons (cpsfy cons))
(define __list (cpsfy list))

(define __MULT (cpsfy *))
(define __PLUS (cpsfy +))
(define ___ (cpsfy -))

(define __EQUAL (cpsfy =))
(define __LESS (cpsfy <))

;; Actor model:
(define (__sleep time cont)
  (inc-uproc-rtime! (current-task)
                    time)
  (&yield-cont cont time))

(define (__self cont)
  (&yield-cont cont (uproc-pid (current-task))))

(define (__node cont)
  ;; TODO
  (&yield-cont cont nil))

(define (__send pid msg cont)
  ;; TODO
  (&yield-cont cont nil))

(define (__recv cont)
  ;; TODO
  (&yield-cont cont nil))

(define (__spawn fun cont)
  ;; TODO
  (&yield-cont cont nil))

;; Misc:
(define __task_info (cpsfy task-info))
