;; The bootstrap code.

(load "compiler/utils.scm")
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

(define (closurize f)
  (&make-closure '()
                 (lambda (env . args)
                   (apply f args))))

(define bootstrap (compose closurize cpsfy))

(define __car (bootstrap car))
(define __cadr (bootstrap car))
(define __cdr (bootstrap cdr))
(define __cddr (bootstrap cddr))

(define __list (bootstrap list))
(define __cons (bootstrap cons))
(define __append (bootstrap append))

(define __MULT (bootstrap *))
(define __PLUS (bootstrap +))
(define ___ (bootstrap -))

(define __EQUAL (bootstrap =))
(define __LESS (bootstrap <))

(define __ref (bootstrap ref))
(define __deref (bootstrap deref))
(define __assignBANG (bootstrap assign!))

;; Actor model:
(define __sleep (bootstrap wait))
(define __self (bootstrap self))
(define __send (bootstrap send))

(define __recv (bootstrap
                (lambda ()
                  (let ((r (recv)))
                    (if (car r)
                        (cdr r)
                        nil)))))

(define __spawn (bootstrap spawn))

;; RBS bootstrap:
(define __assertBANG (bootstrap assert!))
(define __signalBANG (bootstrap signal!))
(define __retractBANG (bootstrap retract!))
(define __select (bootstrap select))

(define __notify_whenever (bootstrap
                           (lambda (who pattern)
                             (whenever pattern
                                       ;; NOTE We can't use FOOF functions, since they yield execution.
                                       (lambda (b)
                                         (send who b))))))

;; Misc:
(define __task_info (bootstrap task-info))
(define __display (bootstrap display))
(define __newline (bootstrap newline))
(define __random (bootstrap random))
