#lang racket

;; Delimited continuation tests...

(require "../testing.rkt")
(require "../../src/runtime/rt.rkt")
(require "../../src/runtime/closures.rkt")
(require "../../src/main.rkt")

(describe
 "delimited continuations"
 (it "Letcc works."
     (assert (run '(letcc _ 23))
             23)
     (assert (run '(letcc k (k 23)))
             23)
     (assert (closure? (run '(letcc k (k k)))))
     (assert (run '(letcc k (* 2 (k 23))))
             23)
     (assert (run '(* 3 (letcc k (* 2 (k 23)))))
             69)
     (assert (run '(* 3 (letcc k (* 2 (k (k 23))))))
             69))

 (it "Shift & Reset work."
     (assert (run '(reset 23))
             23)
     (assert (run '(reset (shift k (k 23))))
             23)
     (assert (run '((reset (shift k (k k))) 23))
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
     (assert (run '(* 10 (reset (* 2 (shift _ (* 5 (shift f (+ (f 1) 1))))))))
             60)
     (assert (run '(let ((f (lambda (x) (shift k (k (k x))))))
                     (+ 1 (reset (+ 10 (f 100))))))
             121)
     (assert (run '(reset
                    (let ((x (shift f (shift f1 (f1 (cons 'a (f '())))))))
                      (shift _ x))))
             '(a))
     (assert (run '(let ((yield (lambda (x)
                                  (shift k (cons x (k '()))))))
                     (reset (do (yield 1) (yield 2) (yield 3) (yield 4) (yield 5)))))
             '(1 2 3 4 5))
     (assert (run '(let ((traverse (lambda (xs)
                                     (letrec ((visit (lambda (xs)
                                                       (if (nil? xs)
                                                           '()
                                                           (visit (shift k
                                                                         (cons (car xs)
                                                                               (k (cdr xs)))))))))
                                       (reset (visit xs))))))
                     (traverse '(1 2 3 4 5))))
             '(1 2 3 4 5))))
