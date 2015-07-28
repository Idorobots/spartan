;; Runtime support for modules.

(define (make-structure . defs)
  ;; FIXME Yeah, an alist... Don't judge me...
  (list make-structure defs))

(define (&structure-ref s f)
  (cdr (assoc f (cadr s))))
