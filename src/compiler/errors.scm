(load "compiler/utils/utils.scm")
(load "compiler/utils/refs.scm")

;; Syntax error

(define (make-compilation-error where what restart)
  (list 'compilation-error where what restart))

(define (compilation-error? e)
  (tagged-list? 'compilation-error e))

(define (compilation-error-location e)
  (get-location (compilation-error-where e)))

(define (compilation-error-where e)
  (cadr e))

(define (compilation-error-what e)
  (caddr e))

(define (compilation-error-restart e)
  (cadddr e))

(define (raise-compilation-error where what)
  (call/cc
   (lambda (cont)
     (raise (make-compilation-error where what cont)))))

;; Error gathering

(define (collect-errors initial-errors thunk)
  (let* ((errors (ref initial-errors))
         (result (with-handlers
                     ((compilation-error?
                       (lambda (error)
                         (push! errors error)
                         ;; NOTE Continue analysis with a special "error" object.
                         ((compilation-error-restart error)
                          (at (compilation-error-location error)
                              (generated
                               (make-error-node
                                (compilation-error-where error))))))))
                   (thunk))))
    (list result (deref errors))))

;; Internal compiler errors

(define (show-stacktrace)
  (for ([s (continuation-mark-set->context (current-continuation-marks))]
        [i (in-naturals)])
    ;; show just the names, not the full source information
    (when (car s) (printf "~s: ~s\n" i s))))

(define (compiler-bug what context)
  (show-stacktrace)
  (error (format "Likely a compiler bug! ~a ~a" what context)))

