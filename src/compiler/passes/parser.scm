;; A very simple parser.

(require "../utils/utils.rkt")
(require "../peggen.rkt")
(require "../env.rkt")
(require "../errors.rkt")
(require "../pass.rkt")
(require "../ast.rkt")

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
                    (make-ast-body (location start end)
                                   exprs
                                   "Bad script"))
                start
                end))))

 '(Expression
   (/ Atom List String Quote))

 '(UnmatchedParen
   (Spacing ")")
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (raise-compilation-error
                 (make-ast-location (location start end))
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
       (matches (make-ast-quote (location start end)
                                 (caddr matching))
                start
                end))))
 '(Quasiquote
   (Spacing "`" Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (make-ast-quasiquote (location start end)
                                      (caddr matching))
                start
                end))))
 '(Unquote
   (Spacing "," Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (make-ast-unquote (location start end)
                                   (caddr matching))
                start
                end))))
 '(UnquoteSplicing
   (Spacing ",@" Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (make-ast-unquote-splicing (location start end)
                                           (caddr matching))
                start
                end))))
 '(UnterminatedQuote
   (Spacing (/ "'" "`" ",@" ","))
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (raise-compilation-error
                 (make-ast-location (location start end))
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
       (matches (make-ast-string (location start end) content)
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
                 (make-ast-location (location start end))
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
       (matches (make-ast-list (location start end)
                               (caddr matching))
                start
                end))))
 '(UnterminatedList
   (Spacing "(" ListContents Spacing EOF)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (raise-compilation-error
                 (make-ast-location (location start end))
                 "Unterminated list, expected a closing `)` to follow:")
                start
                end))))
 '(ListContents
   (* Expression))

 '(Atom
   (/ Symbol Number))

 '(Number
   (Spacing "[+\\-]?[0-9]+(\\.[0-9]+)?")
   (lambda (input result)
     (let* ((matching (match-match result))
            (spacing-start (match-start result))
            (start (car matching))
            (end (match-end result)))
       (matches (make-ast-number (location start end)
                                 (string->number (cadr matching)))
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
       (matches (make-ast-symbol (location start end)
                                 (string->symbol (cadr matching)))
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
                 (make-ast-location (location start end))
                 (format "Invalid symbol `~a` specified at:"
                         (cadr matching)))
                start
                end))))

 '(SymbolContents
   "(([a-zA-Z]|[#!$%*/:<=>?~_^])([a-zA-Z]|[0-9]|[!$%*/:<=>?~_^@]|[+\\-])*|[+\\-])")

 ;; NOTE The above is basically the same as this, except less readable:
 ;; '(SymbolContents
 ;;   (/ (~ SymbolInitial (~ (* SymbolSubsequent))) Sign))

 ;; '(SymbolInitial
 ;;   (/ Alpha Special "#"))

 ;; '(SymbolSubsequent
 ;;   (/ SymbolInitial "@" Digit Sign))

 ;; '(Alpha
 ;;   "[a-zA-Z]")

 ;; '(Special
 ;;   "[!$%*/:<=>?~_^]")

 ;; '(Digit
 ;;   "[0-9]")

 ;; '(Sign
 ;;   "[+\\-]")

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
           (make-ast-primop-app loc
                                '&structure-ref
                                (list acc
                                      (generated
                                       (make-ast-quote loc part)))))
         (make-ast-symbol loc head)
         (map (partial make-ast-symbol loc)
              rest)))

(define parse
  (pass (schema "parse"
                'input non-empty-string?
                'errors a-list?)
        (lambda (env)
          (let ((result (collect-errors (env-get env 'errors)
                                        (lambda ()
                                          (let* ((input (env-get env 'input))
                                                 (parsed (Program input eq-len-hash-input)))
                                            (if (matches? parsed)
                                                (match-match parsed)
                                                (raise-compilation-error
                                                 (make-ast-location (location 0 (string-length input)))
                                                 "Not a valid Spartan file:")))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))
