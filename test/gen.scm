;; Generators

(define (sample gen rand)
  (if (procedure? gen)
      (gen rand)
      gen))

(define (gen-integer min max)
  (lambda (rand)
    (rand min max)))

(define (gen-real min max)
  (lambda (rand)
    (+ min (* (rand) (- max min)))))

(define (gen-number rand)
  ((gen-one-of (gen-integer -1000 1000)
               (gen-real -12345.6 12345.6))
   rand))

(define (gen-string letters gen-max-length)
  (lambda (rand)
    (let ((len (string-length letters))
          (size (sample gen-max-length rand)))
      (list->string
       (map (lambda (_)
              (string-ref letters (rand 0 len)))
            (iota 1 size 1))))))

(define (gen-text gen-max-length)
  (gen-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ()[]{}?-!@#$%^&*~` " gen-max-length))

(define (gen-symbol gen-max-length)
  (compose string->symbol
           (gen-string "abcdefghijklmnopqrstuvwxyz-?" gen-max-length)))

(define gen-valid-symbol
  (gen-symbol (gen-integer 10 20)))

(define (gen-list gen-max-size gen-contents)
  (lambda (rand)
    (let ((size (sample gen-max-size rand)))
      (map (lambda (_)
             (sample gen-contents rand))
           (iota 1 size 1)))))

(define (gen-location rand)
  (let ((start (rand 0 1000)))
    (location start (+ start (rand 0 1000)))))

(define (gen-number-node gen-value)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-number
         (sample gen-value rand)))))

(define (gen-symbol-node gen-symbol)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-symbol
         (sample gen-symbol rand)))))

(define gen-valid-symbol-node
  (gen-symbol-node gen-valid-symbol))

(define (gen-string-node gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-string
         (sample gen-contents rand)))))

(define (gen-list-node gen-max-size)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-list
         (sample (gen-list gen-max-size
                           ;; NOTE To avoid generating huge objects.
                           gen-simple-node)
                 rand)))))

(define (gen-specific-list-node . gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-list
         (map (flip sample rand)
              gen-contents)))))

(define (gen-one-of . alternatives)
  (lambda (rand)
    (sample (list-ref alternatives (rand 0 (length alternatives)))
            rand)))

(define (gen-quote-node gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-quote
         (sample gen-contents rand)))))

(define (gen-quasiquote-node gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-quasiquote
         (sample gen-contents rand)))))

(define (gen-unquote-node gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-unquote
         (sample gen-contents rand)))))

(define (gen-unquote-splicing-node gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-unquote-splicing
         (sample gen-contents rand)))))

(define (gen-specific-const-node gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-const
         (sample gen-contents rand)))))

(define (gen-const-node rand)
  (sample (gen-specific-const-node
           (gen-one-of (gen-number-node gen-number)
                       (gen-string-node (gen-text (gen-integer 0 50)))
                       gen-valid-symbol-node
                       (apply gen-specific-list-node
                              (sample (gen-list (gen-integer 0 3) gen-valid-symbol-node)
                                      rand))))
          rand))

(define (gen-arg-list gen-max-length)
  (gen-list gen-max-length gen-valid-symbol-node))

(define (gen-lambda-node gen-formals gen-body)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-lambda (sample gen-formals rand)
                         (sample gen-body rand)))))

(define (gen-valid-lambda-node rand)
  (sample (gen-lambda-node (gen-arg-list (gen-integer 0 5))
                           gen-simple-node)
          rand))

(define (gen-app-node gen-op . gen-args)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-app (sample gen-op rand)
                      (map (flip sample rand) gen-args)))))

(define (gen-valid-app-node rand)
  (sample (apply gen-app-node
                 gen-simple-node
                 (sample (gen-list (gen-integer 0 5)
                                   gen-simple-node)
                         rand))
          rand))

(define (gen-primop-app-node gen-op . gen-args)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-primop-app (sample gen-op rand)
                             (map (flip sample rand) gen-args)))))

(define (gen-valid-primop-app-node rand)
  (sample (apply gen-primop-app-node
                 gen-valid-symbol
                 (sample (gen-list (gen-integer 0 5)
                                   gen-simple-node)
                         rand))
          rand))

(define (gen-binding-node gen-name gen-value)
  (lambda (rand)
    (let ((val (sample gen-value rand)))
      (at (sample gen-location rand)
          (set-ast-binding-complexity
           (make-ast-binding (sample gen-name rand) val)
           (compute-complexity val))))))

(define (gen-valid-binding-node rand)
  (sample (gen-binding-node gen-valid-symbol-node gen-simple-node)
          rand))

(define (gen-binding-list gen-max-length)
  (gen-list gen-max-length gen-valid-binding-node))

(define (gen-def-node gen-name gen-value)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-def (sample gen-name rand)
                      (sample gen-value rand)))))

(define (gen-valid-def-node rand)
  (sample (gen-def-node gen-valid-symbol-node gen-simple-node)
          rand))

(define (gen-let-node gen-bindings gen-body)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-let (sample gen-bindings rand)
                      (sample gen-body rand)))))

(define (gen-valid-let-node rand)
  (sample (gen-let-node (gen-binding-list (gen-integer 1 5))
                        gen-simple-node)
          rand))

(define (gen-letrec-node gen-bindings gen-body)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-letrec (sample gen-bindings rand)
                         (sample gen-body rand)))))

(define (gen-valid-letrec-node rand)
  (sample (gen-letrec-node (gen-binding-list (gen-integer 1 5))
                           gen-simple-node)
          rand))

(define (gen-fix-node gen-bindings gen-body)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-fix (sample gen-bindings rand)
                      (sample gen-body rand)))))

(define (gen-valid-fix-node rand)
  (sample (gen-fix-node (gen-binding-list (gen-integer 1 5))
                        gen-simple-node)
          rand))

(define (gen-if-node gen-cond gen-then gen-else)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-if (sample gen-cond rand)
                     (sample gen-then rand)
                     (sample gen-else rand)))))

(define (gen-valid-if-node rand)
  (sample (gen-if-node gen-simple-node gen-simple-node gen-simple-node)
          rand))

(define (gen-do-node gen-max-length gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-do
         (sample (gen-list gen-max-length
                           gen-contents)
                 rand)))))

(define (gen-specific-do-node . gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-do
         (map (flip sample rand)
              gen-contents)))))

(define (gen-valid-do-node rand)
  (sample (gen-do-node (gen-integer 1 5) gen-simple-node)
          rand))

(define (gen-body-node gen-max-length gen-contents gen-context)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-body
         (sample (gen-list gen-max-length
                           gen-contents)
                 rand)
         (sample gen-context
                 rand)))))

(define (gen-valid-body-node rand)
  (sample (gen-body-node (gen-integer 1 5) gen-simple-node (gen-text (gen-integer 10 20)))
          rand))

(define (gen-specific-body-node gen-context . gen-contents)
  (lambda (rand)
    (at (sample gen-location rand)
        (make-ast-body
         (map (flip sample rand)
              gen-contents)
         (sample gen-context rand)))))

(define (gen-simple-node rand)
  (sample (gen-one-of (gen-number-node gen-number)
                      gen-valid-symbol-node
                      (gen-string-node (gen-text (gen-integer 0 50))))
          rand))

(define (gen-complex-node rand)
  (sample (gen-one-of (gen-quote-node gen-simple-node)
                      (gen-list-node (gen-integer 0 5))
                      gen-valid-lambda-node
                      gen-non-value-node)
          rand))

(define (gen-value-node rand)
  (sample (gen-one-of (gen-number-node gen-number)
                      (gen-string-node (gen-text (gen-integer 0 50)))
                      (gen-quote-node gen-simple-node))
          rand))

(define (gen-non-value-node rand)
  (sample (gen-one-of gen-valid-do-node
                      gen-valid-if-node
                      gen-valid-app-node
                      gen-valid-primop-app-node
                      gen-valid-let-node
                      gen-valid-letrec-node)
          rand))

(define (gen-misc-node rand)
  (sample (gen-one-of gen-valid-binding-node
                      gen-random-error-node
                      gen-location-node
                      gen-valid-fix-node
                      gen-valid-def-node
                      (gen-quasiquote-node gen-simple-node)
                      (gen-unquote-node gen-simple-node)
                      (gen-unquote-splicing-node gen-simple-node)
                      gen-valid-body-node
                      gen-const-node)
          rand))

(define (gen-ast-node rand)
  (sample (gen-one-of gen-simple-node
                      gen-complex-node
                      gen-value-node
                      gen-non-value-node
                      gen-misc-node)
          rand))

(define (gen-error-node gen-node)
  (lambda (rand)
    (let ((n (sample gen-node rand)))
      (at (ast-node-location n)
          (make-ast-error n)))))

(define gen-random-error-node
  (gen-error-node gen-ast-node))

(define (gen-location-node rand)
  (at (sample gen-location rand)
      (make-ast-location)))

(define (gen-specific-list gen parameters)
  (lambda (rand)
    (map (lambda (p)
           (sample (gen p) rand))
         parameters)))

(define (gen-with-fv gen fv)
  (lambda (rand)
    (let ((vars (sample fv rand)))
      (set-ast-node-free-vars
       (if (set? vars)
           vars
           (apply set vars))
       (sample gen rand)))))

(define (gen-with-bv gen bv)
  (lambda (rand)
    (let ((vars (sample bv rand)))
      (set-ast-node-bound-vars
       (if (set? vars)
           vars
           (apply set vars))
       (sample gen rand)))))

(define (gen-with-fv-bv gen fv bv)
  (gen-with-fv (gen-with-bv gen bv) fv))

(define (gen-with-ctx gen ctx)
  (lambda (rand)
    (set-ast-node-context (sample gen rand) ctx)))

(define (gen-self-recursive gen)
  (lambda (rand)
    (set-ast-binding-self-recursive (sample gen rand) #t)))
