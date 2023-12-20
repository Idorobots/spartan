#lang racket

(require "compiler/env.rkt")
(require "compiler/errors.rkt")
(require "compiler/compiler.rkt")
(require "compiler/utils/utils.rkt")
(require "main.rkt")

(provide run-repl)

(define (print-greeting)
  ;; TODO Version info.
  (displayln ";; Spartan REPL. Type ;help for help."))

(define (print-usage)
  (displayln ";; Input lines to append to the listing.  Edit or add lines by stating the `line# |` prefix.
;;
;; Available commands:
;;  ;help                      Displays this information.
;;  ;q, ;quit, ;exit           Exits the interactive environment.
;;  ;list                      Lists the current listing.
;;  ;clear                     Clears the current listing.
;;  ;run                       Runs the current listings.
;;
;; Available settings:
;;  ;autorun {on|off}          Toggles automatic execution of listings after each entered line. Default = on
;;  ;debug-compiled {on|off}   Toggles debug output of the target compiled code. Default = off"))

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
            (print-prefixed ";; " (compilation-error-what err))
            (loop line listing auto-run show-compiled)))
         ((constantly #t)
          (lambda (err)
            (print-prefixed ";; " (format "Error: ~a" (exn-message err)))
            (show-stacktrace (exn-continuation-marks err))
            (loop line listing auto-run show-compiled))))
      (let ((input (read-line)))
        (case input
          (("")
           (loop line listing auto-run show-compiled))
          ((";help")
           (print-usage)
           (loop line listing auto-run show-compiled))
          ((";q" ";quit" ";exit")
           (exit 0))
          ((";list")
           (displayln ";; Current listing:")
           (print-listing listing)
           (loop line listing auto-run show-compiled))
          ((";clear")
           (displayln ";; Listing cleared.")
           (loop 1 '() auto-run show-compiled))
          ((";run")
           (let ((compiled (c listing)))
             (when show-compiled
               (displayln ";; Compilation result:")
               (print-prefixed ";; " (output->string compiled)))
             (print-prefixed ";; " (output->string (run-code compiled)))
             (loop line listing auto-run show-compiled)))
          ((";autorun on")
           (displayln ";; Enabled auto-run.")
           (loop line listing #t show-compiled))
          ((";autorun off")
           (displayln ";; Disabled auto-run.")
           (loop line listing #f show-compiled))
          ((";debug-compiled on")
           (displayln ";; Enabled compilation output.")
           (loop line listing auto-run #t))
          ((";debug-compiled off")
           (displayln ";; Disabled compilation output.")
           (loop line listing auto-run #f))
          (else
           (let ((new-listing (add-line listing line input)))
             (when auto-run
               (let ((compiled (c new-listing)))
                 (when show-compiled
                   (displayln ";; Compilation result:")
                   (print-prefixed ";; " (output->string compiled)))
                 (print-prefixed ";; " (output->string (run-code compiled)))))
             (loop (+ 1 line) new-listing auto-run show-compiled))))))))
