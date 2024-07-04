// Mutable refs
const makeRef = (v) => ({
  ref: v,
  type: "ref"
});
const isRef = (r) => ((typeof r === "object") && r !== null && !!r.type && r.type === "ref")
const deref = (r) => r.ref;
const assign = (r, v) => ((r.ref = v), r);

// Conses
const makeCons = (car, cdr) => ({
  car: car,
  cdr: cdr,
  type: "cons"
});
const isCons = (c) => (c === null || (typeof c === "object") && !!c.type && c.type === "cons");
const car = (c) => c.car;
const cdr = (c) => c.cdr;

// Closures
const makeClosure = (env, fun) => ({
  fun: fun,
  env: env,
  type: "closure"
});
const isClosure = (c) => ((typeof c === "object") && c !== null && !!c.type && c.type === "closure");
const closureFun = (c) => c.fun;
const closureEnv = (c) => c.env;
const setClosureEnv = (c, e) => (c.env = e, c);

// Continuations
const makeResumable = (cont, hole) => ({
  kont: cont,
  hole: hole,
  type: "resumable"
});
const isResumable = (r) => ((typeof r === "object") && r !== null && !!r.type && r.type === "resumable");
const resume = (r) => r.kont.fun(r.kont.env, r.hole);

// Processes
const gensym = (function () {
  let counter = 0;
  return (symbol) => {
    const ret = symbol + counter;
    counter++;
    return ret;
  };
})();
const makeUproc = (priority, cont, handler, rtime, state) => ({
  continuation: cont,
  delimitedContinuations: null,
  errorHandler: handler,
  state: state,
  rtime: rtime,
  priority: priority,
  pid: gensym("pid"),
  msgQueue: null,
  type: "uproc"
});
const isUproc = (u) => ((typeof u === "object") && u !== null && !!u.type && u.type === "uproc");
const setUprocRtime = (u, ts) => (u.rtime = ts, u);
const setUprocState = (u, s) => (u.state = s, u);
const setUprocContinuation = (u, c) => (u.continuation = c, u);
const setUprocDelimitedContinuations = (u, c) => (u.delimitedContinuations = c, u);
const setUprocErrorHandler = (u, h) => (u.errorHandler = h, u);
const uprocVTime = (u) => (u.rtime * u.priority);
const uprocMsgQueueEmpty = (u) => u.msgQueue === null;
const uprocEnqueueMsg = (u, m) => {
  if (u.msgQueue === null) {
    u.msgQueue = makeCons(m, null);
  } else {
    let q = u.msgQueue;
    while (q.cdr !== null) {
      q = q.cdr;
    }
    q.cdr = makeCons(m, null);
  }
};
const uprocDequeueMsg = (u) => {
  const msg = u.msgQueue.car;
  u.msgQueue = u.msgQueue.cdr;
  return msg;
};

// Structures
const makeStructure = (bindings) => {
  const s = {
    type: "structure"
  };

  bindings.forEach((b) => {
    s[b.name] = b.value;
  });

  return s;
};

const isStructure = (s) => ((typeof s === "object") && s !== null && !!s.type && s.type === "structure");

// Internal procedures
function __write(o) {
  // FIXME Refactor
  switch(typeof o) {
  case "undefined":
    __write("#<void>");
    break;
  case "string":
    process.stdout.write(o);
    break;
  case "object":
    if (o === null) {
      __write("()")
    } else if(isRef(o)) {
      __write("#<ref>");
    } else if(isClosure(o)) {
      __write("#<procedure>");
    } else if(isResumable(o)) {
      __write("#<continuation>");
    } else if(isStructure(o)) {
      __write("#<structure>");
    } else if(isCons(o)) {
      __write("(");
      let i = o;
      while (i !== null) {
        __write(i.car);
        if(i.cdr === null) {
          break;
        } else if (typeof i.cdr === "object" && i.cdr.car !== undefined) {
          __write(" ");
          i = i.cdr;
        } else {
          __write(" . ");
          __write(i.cdr);
          break;
        }
      }
      __write(")");
    }
    break;
  default:
    __write(JSON.stringify(o));
    break;
  }
}

// Bootstrap primitives
const suspend = (thunk) => {
  return makeResumable(
    makeClosure(
      null,
      (e, thunk) => thunk.fun(
        thunk.env,
        makeClosure(null, (e, v) => v)
      )
    ),
    thunk
  );
};

const trampoline = (resumable) => {
  let r = resumable;
  while(isResumable(r)) {
    r = resume(r);
  }
  return r;
};

const delayMilliseconds = (ms) => {
  // FIXME Requires delayMilliseconds to be async as well.
  // await new Promise(resolve => setTimeout(resolve, ms));

  // FIXME This is terrible...
  const start = currentMilliseconds();
  while(currentMilliseconds() < (start + ms));
  return ms;
};

const currentMilliseconds = () => +Date.now();

const display = (v) => {
  __write(v);
  return v;
};

const modulo = (a, b) => {
  return a - b * Math.floor(a / b);
};

// RBS primops
const assertFact = (f) => {
  throw new Exception("RBS is currently not supported.");
};
const signalFact = (f) => {
  throw new Exception("RBS is currently not supported.");
};
const retractFact = (f) => {
  throw new Exception("RBS is currently not supported.");
};
const selectFacts = (q) => {
  throw new Exception("RBS is currently not supported.");
};
const wheneverTrampoline = (pattern, fun) => {
  throw new Exception("RBS is currently not supported.");
};

// Bootstrap procedures
const __yield = makeClosure(null, (e, cont, v, ignored) => makeResumable(cont, v));

const __list = makeClosure(null, (e, ...args) => {
  const c = args[args.length - 1];
  const vals = args.slice(0, args.length - 1);
  const lst = ((rest) => (rest.length === 0)
               ? null
               : makeCons(rest[0], lst(rest.slice(1))));
  return c.fun(c.env, lst(vals));
});
