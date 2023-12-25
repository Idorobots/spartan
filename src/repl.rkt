#lang racket

(require "compiler/utils/io.rkt")
(require "compiler/env.rkt")
(require "compiler/errors.rkt")
(require "compiler/compiler.rkt")
(require "compiler/utils/utils.rkt")
(require "main.rkt")

(provide run-repl)

(define (print-greeting)
  ;; TODO Version info.
  (print-comment "Spartan REPL. Type ;help for help."))

(define (print-usage)
  (print-comment "Input lines to append to the listing.  Edit or add lines by stating the `line# |` prefix.

Available commands:
  ;help                      Displays this information.
  ;q, ;quit, ;exit           Exits the interactive environment.
  ;list                      Lists the current listing.
  ;clear                     Clears the current listing.
  ;run                       Runs the current listings.

Available settings:
  ;autorun {on|off}          Toggles automatic execution of listings after each entered line. Default = on
  ;colors {on|off}           Toggles colored terminal output. Default = on
  ;debug-compiled {on|off}   Toggles debug output of the target compiled code. Default = off"))

(define (print-prompt line)
  (display (format "~a | " line)))

(define (add-line listing line input)
  (cons (cons line input) listing))

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

(define (run-repl env)
  ;; TODO Lax the "unused variable" error.
  ;; TODO Handle Unterminated string/list errors and continue multiline input.
  ;; TODO Handle line number preffixed inputs.
  ;; TODO Color code the various outputs (much like the errors).
  ;; TODO Prefix errors with ;;
  (define (c listing)
    (compile (env-set env 'input (listing->string listing))))

  (print-greeting)
  (let loop ((line 1)
             (listing '())
             (auto-run #t)
             (show-compiled #f))
    (print-prompt line)
    (with-handlers
        ((compilation-error?
          (lambda (err)
            (print-comment (compilation-error-what err))
            (loop line listing auto-run show-compiled)))
         ((constantly #t)
          (lambda (err)
            (print-comment (format "Error: ~a" (exn-message err)))
            (for-each print-comment
                      (get-stacktrace (exn-continuation-marks err)))
            (loop line listing auto-run show-compiled))))
      (let ((input (read-line (current-input-port) 'any)))
        (case input
          (("")
           (loop line listing auto-run show-compiled))
          ((";help")
           (print-usage)
           (loop line listing auto-run show-compiled))
          ((";q" ";quit" ";exit")
           (exit 0))
          ((";list")
           (print-comment "Current listing:")
           (print-listing listing)
           (loop line listing auto-run show-compiled))
          ((";clear")
           (print-comment "Listing cleared.")
           (loop 1 '() auto-run show-compiled))
          ((";run")
           (let ((compiled (c listing)))
             (when show-compiled
               (print-comment "Compilation result:")
               (print-comment (output->string compiled)))
             (print-comment (output->string (run-code compiled)))
             (loop line listing auto-run show-compiled)))
          ((";autorun on")
           (print-comment "Enabled auto-run.")
           (loop line listing #t show-compiled))
          ((";autorun off")
           (print-comment "Disabled auto-run.")
           (loop line listing #f show-compiled))
          ((";colors on")
           (set-color-output #t)
           (print-comment "Enabled colored output.")
           (loop line listing auto-run show-compiled))
          ((";colors off")
           (set-color-output #f)
           (print-comment "Disabled colored output.")
           (loop line listing auto-run show-compiled))
          ((";debug-compiled on")
           (print-comment "Enabled compilation output.")
           (loop line listing auto-run #t))
          ((";debug-compiled off")
           (print-comment "Disabled compilation output.")
           (loop line listing auto-run #f))
          (else
           (let ((new-listing (add-line listing line input)))
             (when auto-run
               (let ((compiled (c new-listing)))
                 (when show-compiled
                   (print-comment "Compilation result:")
                   (print-comment (output->string compiled)))
                 (print-comment (output->string (run-code compiled)))))
             (loop (+ 1 line) new-listing auto-run show-compiled))))))))
