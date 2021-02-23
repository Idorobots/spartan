;; Macro-expander tests.

(define bm (make-builtin-macros))

(describe
 "built-in macros"
 (it "let works properly"
     (assert (macro-expand '(let () (list a b c)) bm)
             '((lambda () (list a b c))))
     (assert (macro-expand '(let ((a 23)) b) bm)
             '((lambda (a) b) 23)))
 (it "let* macro works."
     (assert (macro-expand '(let* ((a 23) (b 5)) c) bm)
             '((lambda (a) ((lambda (b) c) 5)) 23))
     (assert (macro-expand '(let* ((a 23)) a) bm)
             '((lambda (a) a) 23))
     (assert (macro-expand '(let* () a) bm)
             'a)))
