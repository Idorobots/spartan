;; Macro-expander tests.

(define bm (make-builtin-macros))

(describe
 "built-in macros"
 (it "let macro works"
     (assert (macro-expand '(let () (list a b c)) bm)
             '((lambda () (list a b c))))
     (assert (macro-expand '(let ((a 23)) b) bm)
             '((lambda (a) b) 23)))

 (it "let* macro works"
     (assert (macro-expand '(let* ((a 23) (b 5)) c) bm)
             '((lambda (a) ((lambda (b) c) 5)) 23))
     (assert (macro-expand '(let* ((a 23)) a) bm)
             '((lambda (a) a) 23))
     (assert (macro-expand '(let* () a) bm)
             'a))

 (it "handle macro works"
     (assert (macro-expand '(handle expr handler) bm)
             '(call/handler handler
                            (lambda () expr))))

 (it "shift macro works"
     (assert (macro-expand '(shift k (k 23)) bm)
             '(call/shift
               (lambda (k) (k 23)))))

 (it "reset macro works"
     (assert (macro-expand '(reset expr) bm)
             '(call/reset
               (lambda () expr))))

 (it "letcc macro works"
     (assert (macro-expand '(letcc k (k 23)) bm)
             '(call/current-continuation
               (lambda (k) (do (k 23)))))
     (assert (macro-expand '(letcc k (k 23) (k 5)) bm)
             '(call/current-continuation
               (lambda (k) (do (k 23) (k 5)))))))
