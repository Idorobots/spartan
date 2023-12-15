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

(require "runtime/rt.rkt")
(require "rete/rete.rkt")
(provide (all-from-out "runtime/rt.rkt"))
(provide (all-from-out "rete/rete.rkt"))

(define (print-usage)
  (displayln "Usage: sprtn [command] [option]...
Commands:
  compile                       Compiles the supplied Spartan file without executing it.
  exec                          Compiles & executes the supplied Spartan file.

General options:
  --help                        Displays this information.
  -i, --input [filename]        Names the input Spartan file.
  -o, --output [filename]       Names the output file.

Compilation options:
  --phase {parse|expand|alpha|optimize-early|letrec|cps|optimize-late|instrument|closures|hoist|codegen}
                                Selects up to which compilation phase (inclusive) the pipeline will run.

Optimization options:
  -o,--optimize {0|1|2|3}       Selects the level of optimizations (0 = off). Default = 1.
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
  (Spacing (/ "compile" "exec"))
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
        "instrument" "closures" "hoist" "rename" "codegen")
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
      (let* ((init (apply env (match-match parsed)))
             (command (case (env-get init 'command)
                        ((compile) compile)
                        ((exec)    (compose run-code compile)))))
        (unless (env-contains? init 'input-file)
          (command-error "An input file must be specified!"))
        (with-handlers
            ((compilation-error?
              (lambda (e)
                (displayln (compilation-error-what e))))
             ((constantly #t)
              (lambda (e)
                (displayln (format "Aborted due to an error: ~a" e))
                (exit 1))))
          (-> init
              (env-set 'module (env-get init 'input-file))
              (env-set 'input (slurp (env-get init 'input-file)))
              (command)
              (store-result (env-get* init 'output-file 'stdout)))))
      (command-error "Invalid invocation!")))
