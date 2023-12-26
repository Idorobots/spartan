#lang racket

(require "compiler/ast.rkt")
(require "compiler/env.rkt")
(require "compiler/peggen.rkt")
(require "compiler/errors.rkt")
(require "compiler/compiler.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/utils/utils.rkt")
(require "main.rkt")

(provide run-repl)

(define (print-usage env)
  (print-comment
   (format "Input lines to append to the listing.  Edit or add lines by stating the `line# |` prefix.

Available commands:
  ;help                      Displays this information.
  ;q, ;quit, ;exit           Exits the interactive environment.
  ;list                      Lists the current listing.
  ;clear                     Clears the current listing.
  ;run                       Runs the current listings.

Available settings:
  ;color {on|off}            Toggles colored terminal output. Current = ~a
  ;autorun {on|off}          Toggles automatic execution of listings after each entered line. Current = ~a
  ;debug compiled {on|off}   Toggles debug output of the target compiled code. Current = ~a
  ;debug stacktrace {on|off} Toggles debug output of compiler error stacktraces. Current = ~a"
           (toggle-state env 'color)
           (toggle-state env 'autorun)
           (toggle-state env 'show-compiled)
           (toggle-state env 'show-stacktrace))))

(generate-parser
 (ReplCommand
  (/ Help Quit LineEdit List Clear Run Color Autorun Debug)
  (lambda (input result)
    result))

 (Help
  (Spacing ";help" Spacing EOF)
  (constantly (m 'help)))

 (Quit
  (Spacing (/ ";q" ";quit" ";exit") Spacing EOF)
  (constantly (m 'quit)))

 (LineEdit
  (Spacing "[1-9][0-9]{0,2}" Spacing "|" (? "[ ]") ".*" EOF)
  (lambda (input result)
    (let ((match (match-match result)))
      (m 'edit (string->number (nth 1 match)) (nth 5 match)))))

 (List
  (Spacing ";list" Spacing EOF)
  (constantly (m 'list)))

 (Clear
  (Spacing ";clear" Spacing EOF)
  (constantly (m 'clear)))

 (Run
  (Spacing ";run" Spacing EOF)
  (constantly (m 'run)))

 (Color
  (Spacing ";color" WhiteSpace OnOff Spacing EOF)
  (lambda (input result)
    (m 'color (nth 3 (match-match result)))))

 (Autorun
  (Spacing ";autorun" WhiteSpace OnOff Spacing EOF)
  (lambda (input result)
    (m 'autorun (nth 3 (match-match result)))))

 (Debug
  (Spacing ";debug" WhiteSpace (/ "compiled" "stacktrace") WhiteSpace OnOff Spacing EOF)
  (lambda (input result)
    (m (string->symbol
        (string-append "show-"
                       (nth 3 (match-match result))))
       (nth 5 (match-match result)))))

 (OnOff
  (/ "on" "off" InvalidToggle))

 (InvalidToggle
  NonWhiteSpace
  (lambda (input result)
    (let ((match (match-match result)))
      (print-comment (format "Error: Invalid toggle value `~a` specified, expected one of: {on|off}" match))
      'skip)))

 (Spacing
  (: * WhiteSpace))

 (WhiteSpace
  "[ \t\v\r\n]+")

 (NonWhiteSpace
  "[^ \t\v\r\n]+")

 (EOF
  ()))

(define (m . values)
  (matches values 0 0))

(define (print-greeting)
  ;; TODO Version info.
  (print-comment (gray "Spartan REPL. Type ;help for help.")))

(define (toggle-state env toggle)
  (if (env-get env toggle)
      "on"
      "off"))

(define (print-prompt line)
  (display (format "~a ~a " line (yellow "|"))))

(define (add-line env input)
  (let ((line (env-get env 'line))
        (listing (env-get env 'listing)))
    (env-set env
             'listing (cons (cons line input) listing)
             'line (+ 1 line))))

(define (edit-line env line input)
  (env-update env
              'listing
              (lambda (l)
                (cons (cons line input) l))))

(define (listing-line-number l)
  (car l))

(define (listing-line-content l)
  (cdr l))

(define (listing->formatted-string listing fmt)
  (let loop ((acc "")
             (line 1)
             (lines (sort listing
                          (lambda (a b)
                            (< (listing-line-number a)
                               (listing-line-number b))))))
    (if (empty? lines)
        acc
        (let* ((first (car lines))
               (line-num (listing-line-number first))
               (content (listing-line-content first)))
          (cond ((< line line-num)
                 (loop (string-append acc (fmt line "")) (+ 1 line) lines))
                ((= line line-num)
                 (loop (string-append acc (fmt line content)) (+ 1 line) (cdr lines)))
                (else
                 ;; NOTE Multiples of the same line, select the last one (first on the list).
                 (loop acc line (cdr lines))))))))

(define (listing->string listing)
  (listing->formatted-string listing
                             (lambda (line content)
                               (format "~a~n" content))))

(define (print-listing listing)
  (display
   (listing->formatted-string listing
                              (lambda (line content)
                                (format "~a ~a ~a~n" line (yellow "|") content)))))

(define (output->string output)
  (with-output-to-string
    (lambda ()
      (pretty-write output))))

(define (print-prefixed prefix output)
  (let ((lines (string-split output "\n")))
    (display
     (listing->formatted-string (zip (iota 1 (length lines) 1)
                                     lines)
                                (lambda (line content)
                                  (format "~a~a~n" prefix content))))))

(define (print-comment output)
  (print-prefixed (gray ";; ") output))

(define (strip-errors ast)
  (map-ast (lambda (node)
             (if (ast-error? node)
                 (ast-error-expr node)
                 node))
           ast))

(define (endisstr state)
  (if state "Enabled" "Disabled"))

(define (cmdstr command)
  (case command
    ((color) "colored output")
    ((autorun) "autorun")
    ((show-compiled) "compilation output")
    ((show-stacktrace) "stacktrace-output")))

(define (run-repl init)
  (print-greeting)
  (let loop ((env (env-set init
                           'line 2
                           'listing '((1 . "23")) ;; FIXME Needed for now to get the defines working in the REPL.
                           'autorun #t
                           'show-compiled #f
                           'show-stacktrace #f)))
    (print-prompt (env-get env 'line))
    (with-handlers
      (((constantly #t)
        (lambda (err)
          (print-comment (format "Error: ~a" (exn-message err)))
          (when (env-get env 'show-stacktrace)
            (for-each print-comment
                      (get-stacktrace (exn-continuation-marks err))))
          (loop env))))

      (define (run-strict new-env)
        (let* ((input (listing->string (env-get new-env 'listing)))
               (e (env-set new-env
                           'input input
                           'last-phase 'validate))
               (validated (compile e))
               (errors (env-get validated 'errors)))
          (if (not (empty? errors))
              (begin
                ;; Report all the errors.
                (for-each (lambda (error)
                            (let ((location (compilation-error-location error))
                                  (what (compilation-error-what error)))
                              (print-comment (format-error (env-get e 'module)
                                                           input
                                                           location
                                                           what))
                              (print-comment "\n")))
                          (source-order errors))
                (loop env))
              ;; Proceed as normal.
              (let ((compiled (compile (env-set validated
                                                'first-phase 'validate
                                                'last-phase (env-get new-env 'last-phase)))))
                (when (env-get new-env 'show-compiled)
                  (print-comment "Compilation result:")
                  (print-comment (output->string compiled)))
                (print-comment (output->string (run-code compiled)))))))

      (define (run-laxed new-env)
        (if (env-get new-env 'autorun)
            (let* ((input (listing->string (env-get new-env 'listing)))
                   (e (env-set new-env
                               'input input
                               'last-phase 'validate))
                   (validated (compile e))
                   (errors (env-get validated 'errors))
                   (unused (filter (lambda (e)
                                     (string-prefix? (compilation-error-what e) "Unused variable"))
                                   errors))
                   (unterminated (filter (lambda (e)
                                           (let ((what (compilation-error-what e)))
                                             (or (string-prefix? what "Unterminated")
                                                 (string-prefix? what "No expression following"))))
                                         errors))
                   (rest (filter (lambda (e)
                                   (and (not (member e unused))
                                        (not (member e unterminated))))
                                 errors)))
              (cond ((not (empty? rest))
                     ;; Report the remaining errors.
                     (for-each (lambda (error)
                                 (let ((location (compilation-error-location error))
                                       (what (compilation-error-what error)))
                                   (print-comment (format-error (env-get e 'module)
                                                                input
                                                                location
                                                                what))
                                   (print-comment "\n")))
                               (source-order rest))
                     ;; Restart from the last state.
                     (loop env))
                    ((not (empty? unterminated))
                     ;; Continue gathering lines.
                     (loop new-env))
                    (else
                     ;; Proceed as normal, but strip the error object out of the AST.
                     (let ((compiled (compile (env-set validated
                                                       'ast (strip-errors (env-get validated 'ast))
                                                       'errors '()
                                                       'first-phase 'validate
                                                       'last-phase (env-get new-env 'last-phase)))))
                       (when (env-get new-env 'show-compiled)
                         (print-comment "Compilation result:")
                         (print-comment (output->string compiled)))
                       (print-comment (output->string (run-code compiled))))
                     (loop new-env))))
            (loop new-env)))

      (let* ((input (read-line (current-input-port) 'any))
             (result (ReplCommand input)))
        (if (matches? result)
            ;; If it's a command, execute it.
            (match (match-match result)
              ((list 'help)
               (print-usage env)
               (loop env))
              ((list 'quit)
               (exit 0))
              ((list 'edit line input)
               (run-laxed (edit-line env line input)))
              ((list 'list)
               (print-comment "Current listing:")
               (print-listing (env-get env 'listing))
               (loop env))
              ((list 'run)
               (run-strict env))
              ((list 'clear)
               (print-comment "Listing cleared.")
               (loop (env-set env 'line 1 'listing '())))
              ((list command 'skip)
               (loop env))
              ((list command state)
               (when (equal? command 'color)
                 (set-color-output state))
               (print-comment (format "~a ~a" (endisstr state) (cmdstr command)))
               (loop (env-set env command state))))
            ;; Otherwise treat it as code input.
            (run-laxed (add-line env input)))))))
