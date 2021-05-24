;; A very simple parser.

(load-once "compiler/peggen.scm")
(load-once "compiler/env.scm")
(load-once "compiler/pass.scm")
(load-once "compiler/ast.scm")
(load-once "compiler/errors.scm")

;; FIXME Re-generates the parser on each boot of the compiler. Probably super slow.
(generate-parser
 '(Program
   ((+ (/ Expression UnmatchedParen)) Spacing EOF)
   (lambda (input result)
     (let* ((matching (match-match result))
            (exprs (car matching))
            (start (match-start result))
            (end (match-end result)))
       (matches (if (= (length exprs) 1)
                    (car exprs)
                    (at (location start end)
                        (make-ast-body exprs "Bad script")))
                start
                end))))

 '(Expression
   (/ List Atom String Quote))

 '(UnmatchedParen
   (Spacing ")")
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (raise-compilation-error
                 (at (location start end)
                     (make-ast-location))
                 "Unmatched `)`, expected an opening `(` to come before:")
                start
                end))))

 '(Quote
   (/ PlainQuote Quasiquote UnquoteSplicing Unquote UnterminatedQuote))
 '(PlainQuote
   (Spacing "'" Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (location start end)
                    (make-ast-quote (caddr matching)))
                start
                end))))
 '(Quasiquote
   (Spacing "`" Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (location start end)
                    (make-ast-quasiquote (caddr matching)))
                start
                end))))
 '(Unquote
   (Spacing "," Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (location start end)
                    (make-ast-unquote (caddr matching)))
                start
                end))))
 '(UnquoteSplicing
   (Spacing ",@" Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (location start end)
                    (make-ast-unquote-splicing (caddr matching)))
                start
                end))))
 '(UnterminatedQuote
   (Spacing (/ "'" "`" ",@" ","))
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (raise-compilation-error
                 (at (location start end)
                     (make-ast-location))
                 (format "No expression following `~a`:"
                         (cadr matching)))
                start
                end))))

 '(String
   (/ ProperString UnterminatedString))
 '(ProperString
   (Spacing "\"" StringContents "\"")
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result))
            (content (caddr matching)))
       (matches (at (location start end)
                    (make-ast-string content))
                start
                end))))
 '(UnterminatedString
   (Spacing "\"" StringContents EOF)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result))
            (content (caddr matching)))
       (matches (raise-compilation-error
                 (at (location start end)
                     (make-ast-location))
                 "Unterminated string literal, expected a closing `\"` to follow:")
                start
                end))))
 '(StringContents
   "[^\"]*")

 '(List
   (/ ProperList UnterminatedList))
 '(ProperList
   (Spacing "(" ListContents Spacing ")")
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (location start end)
                    (make-ast-list (caddr matching)))
                start
                end))))
 '(UnterminatedList
   (Spacing "(" ListContents Spacing EOF)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (raise-compilation-error
                 (at (location start end)
                     (make-ast-location))
                 "Unterminated list, expected a closing `)` to follow:")
                start
                end))))
 '(ListContents
   (* Expression))

 '(Atom
   (/ Number Symbol))

 '(Number
   (Spacing "[+\\-]?[0-9]+(\\.[0-9]+)?")
   (lambda (input result)
     (let* ((matching (match-match result))
            (spacing-start (match-start result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (location start end)
                    (make-ast-number (string->number (cadr matching))))
                start
                end))))

 '(Symbol
   (/ PlainSymbol StructureRef InvalidSymbol))
 '(PlainSymbol
   (Spacing SymbolContents (! "."))
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (location start end)
                    (make-ast-symbol (string->symbol (cadr matching))))
                start
                end))))
 '(StructureRef
   (Spacing SymbolContents (+ (: ".") SymbolContents) (! "."))
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (expand-structure-refs (location start end)
                                       (string->symbol (cadr matching))
                                       (map (compose string->symbol cadr) (caddr matching)))
                start
                end))))
 '(InvalidSymbol
   (Spacing (~ (+ (/ "." SymbolContents))))
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (raise-compilation-error
                 (at (location start end)
                     (make-ast-location))
                 (format "Invalid symbol `~a` specified at:"
                         (cadr matching)))
                start
                end))))
 '(SymbolContents
   "[^\\(\\)\"'`,\\.@; \t\v\r\n]+")

 '(Spacing
   (: (* (/ "[ \t\v\r\n]+" Comment)))
   (lambda (input result)
     (let ((start (match-start result))
           (end (match-end result)))
       ;; NOTE So that we can skip the spacing later.
       (matches end start end))))
 '(Comment
   (: ";[^\n]*" (/ "\n" EOF)))
 '(EOF
   ()))

(define (expand-structure-refs loc head rest)
  (foldl (lambda (part acc)
           (at loc
               (make-ast-primop-app
                '&structure-ref
                (list acc
                      (at loc
                          (generated
                           (make-ast-quote part)))))))
         (wrap-symbol loc head)
         (map (partial wrap-symbol loc)
              rest)))

(define (wrap-symbol loc s)
  (at loc
      (make-ast-symbol s)))

(define parse
  (pass (schema "parse"
                'input non-empty-string?
                'errors a-list?)
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (let* ((input (env-get env 'input))
                                                 (parsed (Program input)))
                                            (if (matches? parsed)
                                                (match-match parsed)
                                                (raise-compilation-error
                                                 (at (location 0 (string-length input))
                                                     (make-ast-location))
                                                 "Not a valid Spartan file:")))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))
