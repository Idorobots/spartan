;; Module support

(define-syntax define-protocol
  (syntax-rules (declare)
    ((define-protocol name . declarations)
     (define name (let ((info (infere 'declarations)))
                    (hash-set (make-hash)
                              'info (lambda () info)))))))

(define (infere decls)
  (map (lambda (decl)
         (cons (declaration-name decl)
               (declaration-attrs decl)))
       decls))

(define (declaration-name decl)
  (caadr decl))

(define (declaration-attrs decl)
  nil)

(define (make-hash)
  nil)

(define (hash-set table key value)
  (cons (cons key value)
        table))

(define (hash-get table key)
  (let ((v (assoc key table)))
    (if v
        (cdr v)
        nil)))

(define (hash-merge table1 table2)
  (append table1 table2))

(define-syntax define-module
  ;; TODO Append module name to all the identifiers.
  (syntax-rules (declare define provide require)
    ((define-module) nil)
    ((define-module (name) forms ...)
     (begin
       (set! &module-defs nil)
       (define (unload) nil)
       (define (reload) nil)
       (define (load)
         (cons 'name
               (append &module-defs
                       (list (cons 'unload unload)
                             (cons 'reload reload)
                             (cons 'load load)))))
       (define-module forms ...)))
    ((define-module (require module) forms ...)
     ;; TODO Ensure the module is loaded.
     (define-module forms ...))
    ((define-module (provide protocol) forms ...)
     ;; TODO Actually use this knowledge for something...
     (define-module forms ...))
    ((define-module (declare . function) forms ...)
     ;; TODO Actually use this knowledge for something...
     (define-module forms ...))
    ((define-module (define (name arg ...) body ...) forms ...)
     (begin
       ;; TODO Check if its external.
       (push-def! (cons 'name (lambda (arg ...) body ...)))
       (define-module forms ...)))
    ((define-module (define name value) forms ...)
     (begin
       (push-def! (cons 'name value))
       (define-module forms ...)))))

(define &module-defs nil)
(define (push-def! def)
  (set! &module-defs (cons def &module-defs)))

(define (module-call module function . args)
  (apply (hash-get module function) args))
