#lang racket

(require "compiler/ast.rkt")
(require "compiler/env.rkt")
(require "compiler/errors.rkt")
(require "compiler/compiler.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/utils/utils.rkt")
(require "main.rkt")

(provide run-repl)

(define (print-greeting)
  ;; TODO Version info.
  (print-comment (gray "Spartan REPL. Type ;help for help.")))

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
  ;debug-compiled {on|off}   Toggles debug output of the target compiled code. Current = ~a
  ;debug-stacktrace {on|off} Toggles debug output of compiler error stacktraces. Current = ~a"
           (toggle-state env 'color)
           (toggle-state env 'auto-run)
           (toggle-state env 'show-compiled)
           (toggle-state env 'show-stacktrace))))

(define (toggle-state env toggle)
  (if (env-get env toggle)
      "on"
      "off"))

(define (print-prompt line)
  (display (format "~a | " line)))

(define (add-line env input)
  (let ((line (env-get env 'line))
        (listing (env-get env 'listing)))
    (env-set env
             'listing (cons (cons line input) listing)
             'line (+ 1 line))))

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
                                (format "~a | ~a~n" line content)))))

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

(define (run-repl init)
  ;; TODO Handle line number preffixed inputs.
  (print-greeting)
  (let loop ((env (env-set init
                           'line 2
                           'listing '((1 . "23")) ;; FIXME Needed for now to get the defines working in the REPL.
                           'auto-run #t
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

      (define (run new-env)
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
                                       (string-prefix? (compilation-error-what e) "Unterminated"))
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
                 (loop new-env)))))

      (let ((input (read-line (current-input-port) 'any)))
        (case input
          (("")
           (loop env))
          ((";help")
           (print-usage env)
           (loop env))
          ((";q" ";quit" ";exit")
           (exit 0))
          ((";list")
           (print-comment "Current listing:")
           (print-listing (env-get env 'listing))
           (loop env))
          ((";clear")
           (print-comment "Listing cleared.")
           (loop (env-set env 'line 1 'listing '())))
          ((";run")
           (run env))
          ((";autorun on")
           (print-comment "Enabled auto-run.")
           (loop (env-set env 'auto-run #t)))
          ((";autorun off")
           (print-comment "Disabled auto-run.")
           (loop (env-set env 'auto-run #f)))
          ((";color on")
           (set-color-output #t)
           (print-comment "Enabled colored output.")
           (loop (env-set env 'color #t)))
          ((";color off")
           (set-color-output #f)
           (print-comment "Disabled colored output.")
           (loop (env-set env 'color #f)))
          ((";debug-compiled on")
           (print-comment "Enabled compilation output.")
           (loop (env-set env 'show-compiled #t)))
          ((";debug-compiled off")
           (print-comment "Disabled compilation output.")
           (loop (env-set env 'show-compiled #f)))
          ((";debug-stacktrace on")
           (print-comment "Enabled stacktrace output.")
           (loop (env-set env 'show-stacktrace #t)))
          ((";debug-stacktrace off")
           (print-comment "Disabled stacktrace output.")
           (loop (env-set env 'show-stacktrace #f)))
          (else
           (let ((new-env (add-line env input)))
             (if (env-get env 'auto-run)
                 (run new-env)
                 (loop new-env)))))))))
