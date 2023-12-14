#lang racket

;; The command line utility.

(require "compiler/env.rkt")
(require "compiler/peggen.rkt")
(require "compiler/compiler.rkt")
(require "compiler/utils/io.rkt")
(require "compiler/utils/utils.rkt")

(define (print-usage)
  (displayln "Usage: sprtn [option]...
Options:
  --help                        Displays this information.
  -i, --input [filename]        Names the input Spartan file.
  -o, --output [filename]       Names the output file.
  --phase {parse|expand|alpha|optimize-early|letrec|cps|optimize-late|closure|hoist|rename|codegen}
                                Selects up to which compilation phase (inclusive) the pipeline will run.
  -o,--optimize {0|1}           Selects the level of optimizations (0 = off). Default = 1.
  --optimizer {naive|super}     Selects the optimizer implementation. Default = naive.
  --target {r7rs|ECMAScript6}   Selects the compilation target. Default = r7rs.
  -- [rest]                     Passes the remaining arguments to the executed script.

Bug reports & documentation available at <https://www.github.com/Idorobots/spartan>."))

;; NOTE When you have a hammer, every problem looks like a PEG parser.
(generate-parser
 (CommandLineArguments
  ((* Option) (? RestArguments) Spacing EOF)
  (lambda (input result)
    (let ((opts (car (match-match result)))
          (rest (cadr (match-match result))))
      (matches (flatten (list opts rest))
               (match-start result)
               (match-end result)))))

 (Option
  (/ Help Input Output Optimizer Phase Optimize Target InvalidOption))

 (Help
  (Spacing "--help")
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
  (/ (/ "parse" "expand" "alpha" "optimize-early" "letrec" "cps" "optimize-late" "closure" "hoist" "rename" "codegen")
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
  (/ (/ "0" "1") InvalidOptimizationLevel))

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
  (displayln (format "sprtn ~a" input))
  (displayln (format "~a ~a~a"
                     (make-string (string-length prefix) #\space)
                     (make-string start #\space)
                     (red (make-string (- end start) #\^))))
  (newline)
  (exit 1))

(define (command-error f . args)
  (displayln (apply format f args))
  (print-usage)
  (exit 1))

(define (store-result result filename)
  (let ((p (lambda ()
             (if (string? result)
                 (display result)
                 (pretty-print result)))))
    (if (equal? filename 'stdout)
        (p)
        (with-output-to-file filename p))))

(let* ((args (current-command-line-arguments))
       ;; FIXME Kinda redundant to stringify these when they were almost already parsed.
       (input (string-join (vector->list args) " "))
       (parsed (CommandLineArguments input)))
  (if (matches? parsed)
      (let* ((init (apply env (match-match parsed))))
        (unless (env-contains? init 'input-file)
          (command-error "An input file must be specified!"))
        (-> init
            (env-set 'module (env-get init 'input-file))
            (env-set 'input (slurp (env-get init 'input-file)))
            (compile)
            (store-result (env-get* init 'output-file 'stdout))))
      (command-error "Invalid invocation!")))
