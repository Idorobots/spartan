module runtime.rt;

import std.stdio;

import runtime.continuations;


Continuation* hello(void *arg) {
    return continuation(closure(delegate Continuation* (Continuation *next, void *arg) {
                writefln("Hello %s!", *(cast(string*) arg));
                return yield(next, arg);
            }),
        arg);
}

int main(string[] args) {

    Continuation *c;

    foreach(arg; args[1..$]) {
        c = hello(cast(void*) &arg);
        c = apply(c, halt());
    }

    writeln("Result: ", c.hole);

    return 0;
}
