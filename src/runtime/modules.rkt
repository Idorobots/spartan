#lang racket

;; Runtime support for modules.

(provide &make-structure &structure-binding &structure-ref)

(define (&make-structure . defs)
  ;; FIXME Yeah, an alist... Don't judge me...
  (list &make-structure defs))

(define &structure-binding cons)

(define (&structure-ref s f)
  (cdr (assoc f (cadr s))))
