;; Exception handler tests:

;; Can raise an error.
(assert (run '(raise 'error)) 'error)
(assert (run '(raise (* 2 2))) 4)
(assert (run '(* 2 (raise (* 2 2)))) 4)

;; Can handle an error.
(assert (run '(handle (raise 'error)
                      (lambda (e _)
                        'ok)))
        'ok)

;; Can reraise an error.
(assert (run '(handle (raise 'error)
                      (lambda (e _)
                        (raise e))))
        'error)

;; Can handle reraised errors.
(assert (run '(handle (handle (raise 'error)
                              (lambda (e _)
                                (raise e)))
                      (lambda (e _)
                        'ok)))
        'ok)

;; Can restart computation.
(assert (run '(* 2 (handle (raise 3)
                           (lambda (_ restart)
                             (restart 2)))))
        4)

;; Restarted computation has proper handler.
(assert (run '(* 2 (handle (raise (raise 3))
                           (lambda (e restart)
                             (restart (* 2 e))))))
        24)
