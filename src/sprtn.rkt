#lang racket

;; The command line utility.

(require "compiler/env.rkt")
(require "compiler/peggen.rkt")
(require "compiler/errors.rkt")
(require "compiler/compiler.rkt")
(require "compiler/ast/utils.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/utils/utils.rkt")

(require "main.rkt")

(provide (all-from-out "main.rkt"))

(define (print-usage)
  (displayln "Usage: sprtn [command] [option]...
Commands:
  compile                       Compiles the supplied Spartan file without executing it.
  exec                          Compiles & executes the supplied Spartan file.
  repl                          Starts an interactive read eval print loop environment.

General options:
  --help                        Displays this information.
  -i, --input [filename]        Names the input Spartan file.
  -o, --output [filename]       Names the output file.

Compilation options:
  --phase {parse|expand|alpha|optimize-early|letrec|cps|optimize-late|instrument|closures|optimize-final|hoist|rename|codegen}
                                Selects up to which compilation phase (inclusive) the pipeline will run.

Optimization options:
  -o,--optimize {0|1|2|3}       Selects the level of optimizations (0 = off). Default = 2.
  --optimizer {naive|super}     Selects the optimizer implementation. Default = naive.

Code generation options:
  --target {r7rs|ECMAScript6}   Selects the compilation target. Default = r7rs.

Code generation options:
  -- [argument]...              Passes the remaining arguments to the executed script.

Bug reports & documentation available at <https://www.github.com/Idorobots/spartan>."))

;; NOTE When you have a hammer, every problem looks like a PEG grammar.
(generate-parser
 (CommandLineArguments
  (/ Help
     (Command (* Option) (? RestArguments) Spacing EOF)
     MissingCommand)
  (lambda (input result)
    (let* ((cmdline (match-match result))
           (cmd (car cmdline))
           (opts (cadr cmdline))
           (rest (caddr cmdline)))
      (matches (flatten (list cmd opts rest))
               (match-start result)
               (match-end result)))))

 (Command
  (/ Compile InvalidCommand))

 (Compile
  (Spacing (/ "compile" "exec" "repl"))
  (lambda (input result)
    (m result 'command (string->symbol (cadr (match-match result))))))

 (InvalidCommand
  NonWhiteSpace
  (lambda (input result)
    (let ((match (match-match result))
          (start (match-start result))
          (end (match-end result)))
      (option-error "sprtn" input start end
                    "Invalid command `~a` specified, expected one of: {compile|exec}" match))))

 (MissingCommand
  (Spacing EOF)
  (lambda (input result)
    (print-usage)
    (exit 0)))

 (Option
  (/ Help Input Output Optimizer Phase Optimize Target InvalidOption))

 (Help
  (Spacing "--help" (/ WhiteSpace EOF))
  (lambda (input result)
    (print-usage)
    (exit 0)))

 (Input
  (Spacing (/ "-i" "--input") Spacing NonWhiteSpace)
  (lambda (input result)
    (let ((match (cadddr (match-match result))))
      (m result 'input-file match))))

 (Output
  (Spacing (/ "-o" "--output") Spacing NonWhiteSpace)
  (lambda (input result)
    (let ((match (cadddr (match-match result))))
      (m result 'output-file match))))

 (Phase
  (Spacing "--phase" Spacing CompilerPhase)
  (lambda (input result)
    (let ((match (cadddr (match-match result))))
      (m result 'last-phase (string->symbol match)))))

 (CompilerPhase
  (/ (/ "parse" "expand" "alpha" "optimize-early" "letrec" "cps" "optimize-late"
        "instrument" "closures" "optimize-final" "hoist" "rename" "codegen")
     InvalidCompilerPhase))

 (InvalidCompilerPhase
  NonWhiteSpace
  (lambda (input result)
    (let ((match (match-match result))
          (start (match-start result))
          (end (match-end result)))
      (option-error "sprtn" input start end
                    "Invalid compilation phase `~a` specified, expected one of: {parse|expand|alpha|optimize-early|letrec|cps|optimize-late|closure|hoist|rename}" match))))

 (Optimize
  (Spacing (/ "-O" "--optimize") Spacing OptimizationLevel)
  (lambda (input result)
    (let ((match (cadddr (match-match result))))
      (m result 'optimization-level (string->number match)))))

 (OptimizationLevel
  (/ (/ "0" "1" "2" "3") InvalidOptimizationLevel))

 (InvalidOptimizationLevel
  NonWhiteSpace
  (lambda (input result)
    (let ((match (match-match result))
          (start (match-start result))
          (end (match-end result)))
      (option-error "sprtn" input start end
                    "Invalid optimization level `~a` specified, expected one of: {0|1}" match))))

 (Optimizer
  (Spacing "--optimizer" Spacing OptimizerAlgorithm)
  (lambda (input result)
    (let ((match (cadddr (match-match result))))
      (m result 'optimizer (string->symbol match)))))

 (OptimizerAlgorithm
  (/ (/ "naive" "super") InvalidOptimizerAlgorithm))

 (InvalidOptimizerAlgorithm
  NonWhiteSpace
  (lambda (input result)
    (let ((match (match-match result))
          (start (match-start result))
          (end (match-end result)))
      (option-error "sprtn" input start end
                    "Invalid optmizer `~a` specified, expected one of: {naive|super}" match))))

 (Target
  (Spacing "--target" Spacing CodegenTarget)
  (lambda (input result)
    (let ((match (cadddr (match-match result))))
      (m result 'target (string->symbol match)))))

 (CodegenTarget
  (/ (/ "r7rs" "ECMAScript6") InvalidTarget))

 (InvalidTarget
  NonWhiteSpace
  (lambda (input result)
    (let ((match (match-match result))
          (start (match-start result))
          (end (match-end result)))
      (option-error "sprtn" input start end
                    "Invalid compilation target `~a` specified, expected one of: {r7rs|ECMAScript6}" match))))

 (InvalidOption
  (Spacing (! "--" WhiteSpace) NonWhiteSpace)
  (lambda (input result)
    (let ((match (caddr (match-match result)))
          (start (match-start result))
          (end (match-end result)))
      (option-error "sprtn" input start end
                    "Invalid option `~a` specified." match))))

 (RestArguments
  ("--" WhiteSpace Anything)
  (lambda (input result)
    (let ((match (caddr (match-match result))))
      (m result 'rest-args match))))

 (Anything
  ".*")

 (Spacing
  (: * WhiteSpace)
  no-inline)

 (WhiteSpace
  "[ \t\v\r\n]+")

 (NonWhiteSpace
  "[^ \t\v\r\n]+")

 (EOF
  ()))

(define no-inline
  ;; NOTE Prevents inlining of this rule making it hit the cache more often and perform better.
  (lambda (input result)
    result))

(define (m match key value)
  (matches (list key value)
           (match-start match)
           (match-end match)))

(define (option-error prefix input start end f . args)
  (displayln (apply format f args))
  (newline)
  (displayln (format "  sprtn ~a" input))
  (displayln (format "  ~a ~a~a"
                     (make-string (string-length prefix) #\space)
                     (make-string start #\space)
                     (red (make-string (- end start) #\^))))
  (newline)
  (displayln (format "Try `sprtn --help` for usage information."))
  (exit 1))

(define (command-error f . args)
  (displayln (apply format f args))
  (print-usage)
  (exit 1))

(define (store-result result filename)
  (let ((p (lambda ()
             (cond ((string? result)
                    (display result))
                   ((and (env? result)
                         (env-contains? result 'ast))
                    (pretty-write (ast->plain (env-get result 'ast))))
                   ((and (env? result)
                         (env-contains? result 'init))
                    (pretty-write (list 'begin
                                        (map (lambda (kv)
                                               (list 'define
                                                     (car kv)
                                                     (ast->plain (cdr kv))))
                                             (env-get result 'data))
                                        (ast->plain (env-get result 'init)))))
                   (else
                    (pretty-write result))))))
    (if (equal? filename 'stdout)
        (p)
        (with-output-to-file filename p
          #:exists 'replace))))

(let* ((args (current-command-line-arguments))
       ;; FIXME Kinda redundant to stringify these when they were almost already parsed.
       (input (string-join (vector->list args) " "))
       (parsed (CommandLineArguments input)))
  (if (matches? parsed)
      (let* ((init (apply env (match-match parsed))))
        (case (env-get init 'command)
          ;; Just run the compiler.
          ((compile)
           (unless (env-contains? init 'input-file)
             (command-error "An input file must be specified!"))
           (with-handlers
               ((compilation-error?
                 (lambda (e)
                   (displayln (compilation-error-what e))))
                ((constantly #t)
                 (lambda (e)
                   (displayln (format "Compilation aborted due to an error: ~a" e))
                   (exit 1))))
             (-> init
                 (env-set 'module (env-get init 'input-file))
                 (env-set 'input (slurp (env-get init 'input-file)))
                 (compile)
                 (store-result (env-get* init 'output-file 'stdout)))))
          ;; Run the provided script in r7rs target only.
          ((exec)
           (unless (env-contains? init 'input-file)
             (command-error "An input file must be specified!"))
           ;; TODO Check if target is Scheme.
           (with-handlers
               ((compilation-error?
                 (lambda (e)
                   (displayln (compilation-error-what e))))
                ((constantly #t)
                 (lambda (e)
                   (displayln (format "Execution aborted due to an error: ~a" e))
                   (exit 1))))
             (-> init
                 (env-set 'module (env-get init 'input-file))
                 (env-set 'input (slurp (env-get init 'input-file)))
                 (compile)
                 (run-code))))
          ((repl)
           (displayln "Spartan REPL. Type ;help for help.")
           (let loop ((e (env-set init 'module "repl"))
                      (listing "")
                      (show-compiled #f))
             (display ">> ")
             (let ((input (read-line)))
               ;; TODO Add a special syntax for overriding certain lines BASIC style.
               (case input
                 ((";help")
                  (displayln "; Available commands: ;help, ;q[uit], ;exit, ;list, ;compiled {on|off}")
                  (loop e listing show-compiled))
                 ((";q" ";quit" ";exit")
                  (exit 0))
                 ((";list")
                  (displayln "; Current listing:")
                  ;; TODO Prefix with ";" and line numbers
                  (displayln listing)
                  (loop e listing show-compiled))
                 ((";compiled on")
                  (displayln "; Enabling compilation output.")
                  (loop e listing #t))
                 ((";compiled off")
                  (displayln "; Disabling compilation output.")
                  (loop e listing #f))
                 (("")
                  (loop e listing show-compiled))
                 (else
                  (with-handlers
                      ((compilation-error?
                        (lambda (err)
                          (displayln (compilation-error-what err))
                          (loop e listing show-compiled)))
                       ((constantly #t)
                        (lambda (err)
                          (displayln (format "Error: ~a" (exn-message err)))
                          (show-stacktrace (exn-continuation-marks err))
                          (loop e listing show-compiled))))
                    (let* ((new-listing (string-append listing input "\n"))
                           (new-env (env-set e 'input new-listing))
                           (compiled (compile new-env))
                           (result (run-code compiled)))
                      (when show-compiled
                        (displayln "; Compilation result:")
                        ;; TODO Prefix with ";"
                        (pretty-write compiled))
                      (pretty-write result)
                      (loop new-env new-listing show-compiled))))))))))
      (command-error "Invalid invocation!")))
