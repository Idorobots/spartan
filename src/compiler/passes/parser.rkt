#lang racket

;; Language grammar specification and parser.

(require "../utils/utils.rkt")
(require "../peggen.rkt")
(require "../env.rkt")
(require "../errors.rkt")
(require "../pass.rkt")
(require "../ast.rkt")
(require "../modules.rkt")

(provide parse)

;; FIXME Re-generates the parser on each boot of the compiler. Probably super slow.
(generate-parser
 (Program
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

 (Expression
  (/ Atom List String Quote))

 (UnmatchedParen
  (Spacing ")")
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (raise-compilation-error
                (make-ast-location (location start end))
                "Unmatched `)`, expected an opening `(` to come before:")
               start
               end))))

 (Quote
  (/ PlainQuote Quasiquote UnquoteSplicing Unquote UnterminatedQuote))
 (PlainQuote
  (Spacing "'" Expression)
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (make-ast-quote (location start end)
                               (caddr matching))
               start
               end))))
 (Quasiquote
  (Spacing "`" Expression)
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (make-ast-quasiquote (location start end)
                                    (caddr matching))
               start
               end))))
 (Unquote
  (Spacing "," Expression)
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (make-ast-unquote (location start end)
                                 (caddr matching))
               start
               end))))
 (UnquoteSplicing
  (Spacing ",@" Expression)
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (make-ast-unquote-splicing (location start end)
                                          (caddr matching))
               start
               end))))
 (UnterminatedQuote
  (Spacing (/ "'" "`" ",@" ","))
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (raise-compilation-error
                (make-ast-location (location start end))
                (format "No expression following `~a`:"
                        (cadr matching)))
               start
               end))))

 (String
  (/ ProperString UnterminatedString))
 (ProperString
  (Spacing "\"" StringContents "\"")
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result))
           (content (caddr matching)))
      (matches (make-ast-string (location start end) content)
               start
               end))))
 (UnterminatedString
  (Spacing "\"" StringContents EOF)
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result))
           (content (caddr matching)))
      (matches (raise-compilation-error
                (make-ast-location (location start end))
                "Unterminated string literal, expected a closing `\"` to follow:")
               start
               end))))
 (StringContents
  (* (/ UnescapedStringCharacter EscapedStringCharacter))
  (lambda (input result)
    (let ((match (match-match result))
          (start (match-start result))
          (end (match-end result)))
      (matches  (if (every? string? match)
                    (foldr string-append-immutable "" match)
                    ;; FIXME This is a bit redundant, but might make more sense when ${} embeds are implemented.
                    (raise-compilation-error
                     (make-ast-location (location start end))
                     "Invalid string literal:"))
                start
                end))))
 (UnescapedStringCharacter
  (~ (! (/ "\"" "\\")) Any))
 (EscapedStringCharacter
  (/ ValidEscapeSequence InvalidEscapeSequence))
 (ValidEscapeSequence
  ("\\" (/ "\"" "\\" "b" "f" "n" "r" "t" "v" (~ "u" HexDigit HexDigit HexDigit HexDigit)))
  (lambda (input result)
    (matches (unescape (cadr (match-match result)))
             (match-start result)
             (match-end result))))

 (InvalidEscapeSequence
  ("\\" Any)
  (lambda (input result)
    (let ((start (match-start result))
          (end (match-end result)))
      (matches (raise-compilation-error
                (make-ast-location (location start end))
                "Invalid escape sequence in string literal, did you mean `\\\\`?")
               start
               end))))

 (List
  (/ ProperList UnterminatedList))
 (ProperList
  (Spacing "(" ListContents Spacing ")")
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (make-ast-list (location start end)
                              (caddr matching))
               start
               end))))
 (UnterminatedList
  (Spacing "(" ListContents Spacing EOF)
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (raise-compilation-error
                (make-ast-location (location start end))
                "Unterminated list, expected a closing `)` to follow:")
               start
               end))))
 (ListContents
  (* Expression))

 (Atom
  (/ Symbol Number))

 (Number
  (Spacing (~ (? Sign) (~ (+ Digit)) (? (~ "." (~ (+ Digit))))))
  (lambda (input result)
    (let* ((matching (match-match result))
           (spacing-start (match-start result))
           (start (match-start result))
           (end (match-end result)))
      (matches (make-ast-number (location start end)
                                (string->number (cadr matching)))
               start
               end))))

 (Symbol
  (/ PlainSymbol StructureRef InvalidSymbol))
 (PlainSymbol
  (Spacing SymbolContents (! "."))
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (make-ast-symbol (location start end)
                                (string->symbol (cadr matching)))
               start
               end))))
 (StructureRef
  (Spacing SymbolContents (+ (: ".") SymbolContents) (! "."))
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (expand-structure-refs (location start end)
                                      (string->symbol (cadr matching))
                                      (map (compose string->symbol cadr) (caddr matching)))
               start
               end))))
 (InvalidSymbol
  (Spacing (~ (+ (/ "." SymbolContents))))
  (lambda (input result)
    (let* ((matching (match-match result))
           (start (match-start result))
           (end (match-end result)))
      (matches (raise-compilation-error
                (make-ast-location (location start end))
                (format "Invalid symbol `~a` specified at:"
                        (cadr matching)))
               start
               end))))

 (SymbolContents
  (~ SymbolInitial (~ (* SymbolSubsequent))))

 (SymbolInitial
   (/ Alpha Special (rx "\\p{L}")))

 (SymbolSubsequent
   (/ SymbolInitial Digit (rx "\\p{N}") (rx "\\p{S}")))

 (Alpha
   (rx "[a-zA-Z]")
   ;; FIXME This one seems to be slower than the remaining PEG vs RegEx cases.
   ;; FIXME It's also way less convenient.
   ;; (/ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
   ;;    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
   )

 (Special
   ;; (rx "[!$%*/:<=>?~_^|&@#+\\-]")
   (/ "!" "$" "%" "*" "/" ":" "<" "=" ">" "?" "~" "_" "^" "|" "&" "@" "#" Sign))

 (Digit
  ;; (rx "[0-9]")
  (/ "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

 (HexDigit
  ;; (rx "[0-9a-fA-F]")
  (/ "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f" "A" "B" "C" "D" "E" "F"))

 (Sign
  (/ "+" "-"))

 (Any
  (rx "."))

 (Spacing
  (: (* (/ " " "\t" "\v" "\r" "\n" Comment)))
  no-inline)

 (Comment
  (~ ";" (rx "[^\n]*") (/ "\n" EOF)))
 (EOF
  ()))

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
                                                 (make-ast-location (location 0 (string-length input)))
                                                 "Not a valid Spartan file:")))))))
            (env-set env
                     'ast (car result)
                     'errors (cadr result))))))

(define no-inline
  ;; NOTE Prevents inlining of this rule making it hit the cache more often and perform better.
  (lambda (input result)
    result))

(define (unescape sequence)
  (match sequence
    ("\\" "\\")
    ("\"" "\"")
    ("b" "\b")
    ("f" "\f")
    ("n" "\n")
    ("r" "\r")
    ("t" "\t")
    ("v" "\v")
    (else ;; NOTE Unicode escape
     (-> sequence
         (substring 1)
         (string->number 16)
         (integer->char)
         (string)))))
