;; The bootstrap code.

(load-once "compiler/utils/utils.scm")
(load-once "compiler/utils/refs.scm")
(load-once "rete/rete.scm")

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

;; Built-in primops

(define nil? null?)

;; Basic

(define __nil '())
(define __true #t)
(define __false #f)
(define __not (bootstrap not))

(define __car (bootstrap car))
(define __cadr (bootstrap car))
(define __cdr (bootstrap cdr))
(define __cddr (bootstrap cddr))

(define __list (bootstrap list))
(define __cons (bootstrap cons))
(define __append (bootstrap append))
(define __concat (bootstrap append))

(define __equalQUEST (bootstrap equal?))
(define __nilQUEST (bootstrap nil?))
(define __emptyQUEST (bootstrap empty?))

(define __MULT (bootstrap *))
(define __PLUS (bootstrap +))
(define ___ (bootstrap -))
(define __DIV (bootstrap /))
(define __zeroQUEST (bootstrap zero?))

(define __modulo (bootstrap modulo))
(define __quotient (bootstrap quotient))
(define __remainder (bootstrap remainder))

(define __EQUAL (bootstrap =))
(define __LESS (bootstrap <))
(define __LESSEQUAL (bootstrap <=))
(define __GREATER (bootstrap >))
(define __GREATEREQUAL (bootstrap >=))

(define __ref (bootstrap ref))
(define __deref (bootstrap deref))
(define __assignBANG (bootstrap assign!))

;; Continuations:
(define __callDIVcurrent_continuation (closurize
                                       (lambda (f cont)
                                         (&apply f (closurize
                                                    (lambda (v _)
                                                      (&apply cont v)))
                                                 cont))))

(define __callDIVreset (closurize
                        (lambda (f cont)
                          (&push-delimited-continuation! cont)
                          (&apply f
                                  (closurize
                                   (lambda (v)
                                     (&apply (&pop-delimited-continuation!)
                                             v)))))))

(define __callDIVshift (closurize
                        (lambda (f cont)
                          (&apply f
                                  (closurize
                                     (lambda (v ct2)
                                       (&push-delimited-continuation! ct2)
                                       (&apply cont v)))
                                  (closurize
                                   (lambda (v)
                                     (&apply (&pop-delimited-continuation!)
                                             v)))))))

;; Exceptions:
(define __callDIVhandler (closurize
                          (lambda (handler f cont)
                            (let* ((curr-handler (&error-handler))
                                   (new-handler (closurize
                                                 (lambda (error restart)
                                                   (&set-error-handler! curr-handler)
                                                   (&apply handler error restart cont)))))
                              (&set-error-handler! new-handler)
                              (&apply f
                                      (closurize
                                       (lambda (v)
                                         (&set-error-handler! curr-handler)
                                         (&apply cont v))))))))

(define __raise (closurize
                 (lambda (e cont)
                   (let ((curr-handler (&error-handler)))
                     (&apply curr-handler
                             e
                             (closurize
                              (lambda (v _)
                                (&set-error-handler! curr-handler)
                                (&apply cont v))))))))

;; Actor model:
(define __sleep (bootstrap wait))
(define __self (bootstrap self))
(define __send (bootstrap send))
(define __spawn (bootstrap spawn))

(define __recv (closurize
                (lambda (cont)
                  (let ((r (recv)))
                    (if (car r)
                        ;; If a message is received, return the message.
                        (&yield-cont cont (cdr r))
                        ;; Else, setup a continuation that attempts to re-fetch the message.
                        (&yield-cont (closurize
                                      (lambda (_)
                                        (&apply __recv cont)))
                                     '()))))))

(define __task_info (bootstrap task-info))
(define __monitor (bootstrap (lambda (timeout)
                               (task-info)
                               (&apply __sleep timeout (bootstrap (lambda _
                                                                    (&apply __monitor timeout id)))))))

;; RBS bootstrap:
(define __assertBANG (bootstrap assert!))
(define __signalBANG (bootstrap signal!))
(define __retractBANG (bootstrap retract!))
(define __select (bootstrap select))

(define (notify-whenever who pattern)
  (whenever pattern
            ;; FIXME We can't use Spartan functions, since they yield execution.
            (lambda (b)
              (send who b))))

(define __notify_whenever (bootstrap notify-whenever))

;; Misc:
(define __display (bootstrap display))
(define __newline (bootstrap newline))
(define __random (bootstrap random))
(define __debug (bootstrap (lambda args
                             (pretty-print args)
                             (newline))))
