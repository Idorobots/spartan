<h1><img align="left" alt="foof" src="images/foof.png" height="100" />Spartan</h1>

Spartan is a small Lisp dialect that serves me as a test bed for programming language features.

Currently implemented features are:
- executable language grammar defined as a (PEG parser generator)-generated parser,
- recursion by way of `letrec` conversion with strongly conected components reordering & assignment conversions,
- a first-class module system with a shorthand accessor syntax (`module`, `structure` and `foo.bar`).
- continuations - both delimited (`shift` & `reset`) as well as undelimited (`letcc`),
- exception handling with restarts (`raise` & `handle`) relying on the continuations,
- actor model (`spawn`, `send`, `recv`),
- a built-in, Rete-based Rule Based System (`signal!`, `assert!`, `retract!`, `select` and `notify-whenever`),
- two compilation targets (`ES6`, `r7rs`),
- extensive compilation error handling with precise error locations and colored terminal output,
- a CLI tool `sprtn` with support for `compile`, `run` and `repl` commands,
- an interactive REPL with a twist.

See [here](examples) for some usage examples. You can run these examples with:

<p align="center"><img alt="hello" src="images/hello.gif" /></p

There are also numerous [test files](test/data/errors) available showcasing the compilation error handling features:

<p align="center"><img alt="errors" src="images/errors.gif" /></p>


## Compiler

Run `./build.sh` to build the compiler CLI.

The compiler is far from being useful, it performs only rudimentary low-hanging fruit optimizations & validations. It loosely follows the nanopass framework, with currently implemented passes being:

- parsing,
- built-in macro expansion,
- syntax tree elaboration,
- implicit body handling,
- quasiquote expansion,
- constant value annotation,
- free-variable annotation,
- binding form analysis,
- syntax tree validation,
- alpha conversion,
- built-in function inlining,
- user function inlining,
- constant propagation,
- constant folding,
- common subexpression elimination,
- copy propagation,
- dead code elimination,
- `letrec` binding reordering,
- `letrec` conversion,
- continuation passing style transformation,
- flat closure conversion,
- global value hoisting,
- target-safe name mangling,
- target code generation.

## REPL

The REPL supports a traditional flow of read-eval-print - each line entered is executed automatically and the results are printed immediately:

```
$ ./sprtn repl
;; Spartan REPL. Type ;help for help.
2 | (define (greet who)
3 |   (display "Hello ")
4 |   (display who)
5 |   (newline))
;; #<void>
6 | (greet "world!")
Hello world!
;; #<void>
```

There's also a _basic_ twist - the REPL accepts a full listing and allows editing it line by line:

```
;; Spartan REPL. Type ;help for help.
2 | ;autorun off
;; Disabled autorun.
2 | 1 | (print "Hello world!")
2 | 2 | (goto 1)
3 | ;list
;; Current listing:
1 | (print "Hello world!")
2 | (goto 1)
3 | ;run
;; repl(1,1): Undefined variable `print`, did you mean `list`:
;;   1 | (print "Hello world!")
;;     |  ^^^^^
;;   2 | (goto 1)
;;   3 |
;; repl(2,1): Undefined variable `goto`, did you mean `not`:
;;   1 | (print "Hello world!")
;;   2 | (goto 1)
;;     |  ^^^^
;;   3 |
3 | 2 | (newline)
3 | ;list
;; Current listing:
1 | (print "Hello world!")
2 | (newline)
3 | 1 | (display "Hello world!")
3 | ;list
;; Current listing:
1 | (display "Hello world!")
2 | (newline)
3 | ;run
Hello world!
;; #<void>
```
