;; A very simple parser.

(load "compiler/utils.scm")
(load "compiler/peggen.scm")
(load "compiler/tree-ast.scm")

;; FIXME Re-generates the parser on each boot of the compiler. Probably super slow.
(generate-parser
 '(Program
   ;; FIXME This is pretty awkward, since the "full program" is still a single expression and the rest
   ;; FIXME of the compiler still expects to receive that instead of a list of top level expressions.
   ((? Expression) (* (/ UnmatchedParen Expression)) Spacing EOF)
   (lambda (input result)
     (let* ((matching (match-match result))
            (expr (car matching))
            (trailing (cadr matching))
            (start (match-start result))
            (end (match-end result)))
       (matches (cond ((empty? trailing)
                       expr)
                      ((empty? expr)
                       (at (parse-location start end)
                           (make-list-node trailing)))
                      (else
                       (at (parse-location start end)
                           (make-list-node (cons expr trailing)))))
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
       (matches (at (parse-location start end)
                    (make-unmatched-token-node (cadr matching)))
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
       (matches (at (parse-location start end)
                    (make-quote-node (caddr matching)))
                start
                end))))
 '(Quasiquote
   (Spacing "`" Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (parse-location start end)
                    (make-quasiquote-node (caddr matching)))
                start
                end))))
 '(Unquote
   (Spacing "," Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (parse-location start end)
                    (make-unquote-node (caddr matching)))
                start
                end))))
 '(UnquoteSplicing
   (Spacing ",@" Expression)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (parse-location start end)
                    (make-unquote-splicing-node (caddr matching)))
                start
                end))))
 '(UnterminatedQuote
   (Spacing (/ "'" "`" ",@" ","))
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (parse-location start end)
                    (make-unterminated-quote-node (cadr matching)))
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
       (matches (at (parse-location start end)
                    (make-string-node content))
                start
                end))))
 '(UnterminatedString
   (Spacing "\"" StringContents EOF)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result))
            (content (caddr matching)))
       (matches (at (parse-location start end)
                    (make-unterminated-string-node content))
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
       (matches (at (parse-location start end)
                    (make-list-node (caddr matching)))
                start
                end))))
 '(UnterminatedList
   (Spacing "(" ListContents Spacing EOF)
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (parse-location start end)
                    (make-unterminated-list-node (caddr matching)))
                start
                end))))
 '(ListContents
   (* Expression))

 '(Atom
   (/ Symbol Number))
 '(Number
   (Spacing "[+\\-]?[0-9]+(\\.[0-9]*)?")
   (lambda (input result)
     (let* ((matching (match-match result))
            (spacing-start (match-start result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (parse-location start end)
                    (make-number-node (string->number (cadr matching))))
                start
                end))))
 '(Symbol
   (Spacing (! Number) "[^\\(\\)\"'`,@; \t\v\r\n]+")
   (lambda (input result)
     (let* ((matching (match-match result))
            (start (car matching))
            (end (match-end result)))
       (matches (at (parse-location start end)
                    (make-symbol-node (string->symbol (caddr matching))))
                start
                end))))

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

(define (parse input)
  (let ((result (Program input)))
    (if (matches? result)
        (match-match result)
        (error (format "Could not parse input: ~a" input)))))
