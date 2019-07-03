;; Delimited continuation tests...

(assert (run '(reset 23))
        23)

(assert (run '(reset (shift k (k 23))))
        23)

(assert (run '(* 2 (reset (+ 1 (shift k (k 5))))))
        12)

(assert (run '(* 2 (reset (shift k (+ 1 (k 23))))))
        48)

(assert (run '(reset (* 2 (shift k (+ 1 (k 23))))))
        47)

(assert (run '(reset (* 2 (shift k (k (k 4))))))
        16)

(assert (run '(+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
        117)

(assert (run '(* 10 (reset (* 2 (shift g (* 5 (shift f (+ (f 1) 1))))))))
        60)

(assert (run '(let ((f (lambda (x) (shift k (k (k x))))))
                (+ 1 (reset (+ 10 (f 100))))))
        121)

(assert (run '(reset
               (let ((x (shift f (shift f1 (f1 (cons 'a (f '())))))))
                 (shift g x))))
        '(a))
