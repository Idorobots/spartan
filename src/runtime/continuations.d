module runtime.continuations;


alias ClosureFunction = Continuation* delegate(Continuation *success, void *arg);

struct Closure {
    ClosureFunction fun;
}

struct Continuation {
    Closure *closure;
    void *hole;
}

Closure* closure(ClosureFunction f) {
    auto c = new Closure();
    c.fun = f;
    return c;
}

Continuation* continuation(Closure *c, void* hole) {
    auto cont = new Continuation();
    cont.closure = c;
    cont.hole = hole;
    return cont;
}

Continuation* yield(Continuation *cont, void *arg) {
    cont.hole = arg;
    return cont;
}

Continuation* halt() {
    return continuation(closure(delegate Continuation* (Continuation *ignored, void *result) {
                return continuation(null, result);
            }),
        null);
}

bool isHalted(Continuation* c) {
    return c.closure is null;
}

Continuation* apply(Continuation *cont, Continuation *next) {
    return cont.closure.fun(next, cont.hole);
}
