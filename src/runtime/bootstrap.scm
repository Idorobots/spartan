;; The bootstrap code.

(load "runtime/rt.scm")
(load "rete/rete.scm")

;; Built-in values:
(define __nil nil)

;; Built-in functions:
(define (cpsfy f)
  (lambda args
    (&yield-cont (last args)
                 (apply f
                        (take args
                              (- (length args) 1))))))

(define __car (cpsfy car))
(define __cadr (cpsfy car))
(define __cdr (cpsfy cdr))
(define __cddr (cpsfy cddr))

(define __cons (cpsfy cons))
(define __list (cpsfy list))

(define __MULT (cpsfy *))
(define __PLUS (cpsfy +))
(define ___ (cpsfy -))

(define __EQUAL (cpsfy =))
(define __LESS (cpsfy <))

;; Actor model:
(define __sleep (cpsfy wait))
(define __self (cpsfy self))
(define __send (cpsfy send))

(define (__recv cont)
  (let ((r (recv)))
    (if (car r)
        (&yield-cont cont (cdr r))
        (&yield-cont (lambda (_)
                       ;; NOTE Retry receive.
                       (__recv cont))
                     nil))))

(define __spawn (cpsfy spawn))

;; Module system bootstrap:
(define __make_structure (cpsfy make-structure))

;; RBS bootstrap:
(define __assertBANG (cpsfy assert!))
(define __signalBANG (cpsfy signal!))
(define __retractBANG (cpsfy retract!))
(define __select (cpsfy select))

(define (__notify_whenever who pattern cont)
  (&yield-cont cont
               (whenever pattern
                         ;; NOTE We can't use FOOF functions, since they yield execution.
                         (lambda (b)
                           (send who b)))))

;; Misc:
(define __task_info (cpsfy task-info))
(define __display (cpsfy display))
(define __newline (cpsfy newline))
(define __random (cpsfy random))
