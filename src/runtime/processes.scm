;; Actor model:

(require "../compiler/utils/utils.rkt")

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
  #:constructor-name make-uproc)

(define (uproc priority cont handler rtime state)
  (make-uproc cont '() handler state rtime priority (gensym 'pid) '()))

(define (inc-uproc-rtime! uproc by)
  (set-uproc-rtime! uproc (+ by (uproc-rtime uproc))))

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
