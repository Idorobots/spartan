![FOOF](/flask.png)

# FOOF

1. Dioxygen difluoride - a highly reactive, disruptive and dangerous compound with no apparent application.
2. The sound FOOF makes when it reacts with pretty much anything.

`FOOF` is a small Lisp dialect that serves me as a test bed for programming language features. Currently implemented features are:

- recursion by way of `letrec` & assignment conversions,
- a first-class module system with a shorthand accessor syntax (`module`, `structure` and `foo.bar`).
- continuations - both delimited (`shift` & `reset`) as well as undelimited (`letcc`),
- exception handling with restarts (`raise` & `handle`) relying on the continuations,
- actor model (`spawn`, `send`, `recv`),
- a built-in, Rete-based Rule Based System (`signal!`, `assert!`, `retract!`, `select` and `notify-whenever`),

See [here](src/foof) for some usage examples.

The compiler is far from being useful, it doesn't even have a parser or a code generator yet, and it definitely doesn't optimize anything. It loosely follows the nanopass framework, with currently implemented passes being:

- syntax desugaring,
- built-in macro expansion,
- `letrec` conversion by way of strongly connected components reordering and assignment conversion,
- continuation passing style transformation,
- flat closure conversion.
