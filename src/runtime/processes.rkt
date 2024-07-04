#lang racket

;; Actor model:

(require "../compiler/utils/gensym.rkt")

(provide (struct-out uproc) make-uproc uproc-vtime
         uproc-enqueue-msg! uproc-dequeue-msg! uproc-msg-queue-empty?)

(struct uproc
  ((continuation #:mutable)
   (delimited-continuations #:mutable)
   (error-handler #:mutable)
   (state #:mutable)
   (rtime #:mutable)
   priority
   pid
   (msg-queue #:mutable))
  #:transparent
  #:constructor-name uproc-ctor)

(define (make-uproc priority cont handler rtime state)
  (uproc-ctor cont '() handler state rtime priority (gensym 'pid) '()))

(define (uproc-vtime uproc)
  (* (uproc-rtime uproc)
     (uproc-priority uproc)))

(define (uproc-enqueue-msg! uproc msg)
  ;; FIXME Gross...
  (set-uproc-msg-queue! uproc
                        (append (uproc-msg-queue uproc)
                                (list msg))))

(define (uproc-dequeue-msg! uproc)
  (let* ((q (uproc-msg-queue uproc))
         (msg (car q)))
    (set-uproc-msg-queue! uproc (cdr q))
    msg))

(define (uproc-msg-queue-empty? uproc)
  (empty? (uproc-msg-queue uproc)))
