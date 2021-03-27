;; Internal errors

(define (show-stacktrace)
  (for ([s (continuation-mark-set->context (current-continuation-marks))]
        [i (in-naturals)])
    ;; show just the names, not the full source information
    (when (car s) (printf "~s: ~s\n" i s))))

(define (compiler-bug . what)
  (show-stacktrace)
  (error "Likely a compiler bug!" what))

