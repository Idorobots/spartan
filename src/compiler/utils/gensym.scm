;; Gensym impl

(load-once "compiler/utils/refs.scm")

(define *gensym-counter* (ref 0))
(define (gensym root)
  (assign! *gensym-counter* (+ 1 (deref *gensym-counter*)))
  (string->symbol (string-append (symbol->string root)
                                 (number->string (deref *gensym-counter*)))))

(define (gensym-reset!)
  (assign! *gensym-counter* 0))
