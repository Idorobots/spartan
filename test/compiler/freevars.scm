;; Free vars computation.

;; Values have no free vars.
(assert (free-vars '())
        '())

(assert (free-vars ''(list foo 23))
        '())

(assert (free-vars '23)
        '())

;; Unquoted symbols are a free var.
(assert (free-vars 'foo)
        '(foo))

(assert (free-vars '(list 23 foo))
        '(foo list))

(assert (free-vars '(list 23 'foo))
        '(list))

;; Syntax forms work fine
(assert (free-vars '(if foo bar baz))
        '(bar baz foo))

(assert (free-vars '(do a b (do c d)))
        '(a b c d))

(assert (free-vars '(set! foo bar))
        '(bar))

(assert (free-vars '(reset bar))
        '(bar))

(assert (free-vars '(raise error))
        '(error))

(assert (free-vars '(handle expression handler))
        '(expression handler))

;; Bindings are correctly handled.
(assert (free-vars '(lambda (foo) (list 23 foo)))
        '(list))

(assert (free-vars '(lambda (bar) (list 23 foo)))
        '(foo list))

(assert (free-vars '(let ((f (lambda ()
                               (even? 5))))
                      (let ((t (f)))
                        t)))
        '(even?))

(assert (free-vars '(letcc k (enqueue k)))
        '(enqueue))

(assert (free-vars '(shift k (foo (k bar))))
        '(bar foo))
