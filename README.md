![FOOF](/flask.png)

# FOOF

1. Dioxygen difluoride - a highly reactive, disruptive and dangerous compound with no apparent application.
2. The sound FOOF makes when it reacts with pretty much anything.

`FOOF` is a small Lisp dialect that serves me as a test bed for programming language features. Currently implemented features are:

- executable language grammar defined as a (PEG parser generator)-generated parser,
- recursion by way of `letrec` conversion with strongly conected components reordering & assignment conversions,
- a first-class module system with a shorthand accessor syntax (`module`, `structure` and `foo.bar`).
- continuations - both delimited (`shift` & `reset`) as well as undelimited (`letcc`),
- exception handling with restarts (`raise` & `handle`) relying on the continuations,
- actor model (`spawn`, `send`, `recv`),
- a built-in, Rete-based Rule Based System (`signal!`, `assert!`, `retract!`, `select` and `notify-whenever`),

See [here](test/foof) for some usage examples.

The compiler is far from being useful, it doesn't even have a code generator yet, and it performs only rudimentary low-hanging fruit optimizations. It loosely follows the nanopass framework, with currently implemented passes being:

- parsing,
- built-in macro expansion,
- syntax tree elaboration,
- implicit body handling,
- quasiquote expansion,
- constant value annotation,
- free-variable annotation,
- binding form analysis,
- syntax tree validation,
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
- target-safe name mangling.
