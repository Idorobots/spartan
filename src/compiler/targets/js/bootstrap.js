// Internal procedures
function __apply(c, ...args) {
  return c.fun(c.env, ...args);
}

function __apply_cont(k, h) {
  return { kont: k, hole: h };
}

function __write(o) {
  switch(typeof o) {
  case "string":
    process.stdout.write(o);
    break;
  case "object":
    if (o === null) {
      __write("()")
    } else if(o.ref !== undefined) {
      __write("#<ref>")
    } else if(o.fun !== undefined) {
      __write("#<procedure>")
    } else if(o.kont !== undefined) {
      __write("#<continuation>")
    } else if(o.struct !== undefined) {
      __write("#<structure>")
    } else if(o.car !== undefined) {
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

// Bootstrap values
const __nil = null;

// Bootstrap procedures
const __display = {
  fun: (e, v, c) => {
    __write(v);
    return __apply_cont(c, null);
  }
};
const __newline = {
  fun: (e, c) => {
    __write("\n");
    return __apply_cont(c, null);
  }
};
const __PLUS = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a + b));
  }
};
const ___ = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a - b));
  }
};
const __MULT = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a * b));
  }
};
const __DIV = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a / b));
  }
};
const __modulo = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a % b));
  }
};
const __quotient = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, Math.floor(a/b));
  }
};
const __LESS = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a < b));
  }
};
const __LESSEQUAL = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a <= b));
  }
};
const __GREATER = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a > b));
  }
};
const __GREATEREQUAL = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a >= b));
  }
};
const __EQUAL = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a == b));
  }
};
const __cons = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, { car: a, cdr: b });
  }
};
const __car = {
  fun: (e, l, c) => {
    return __apply_cont(c, l.car);
  }
};
const __cdr = {
  fun: (e, l, c) => {
    return __apply_cont(c, l.cdr);
  }
};
const __list = {
  fun: (e, ...args) => {
    const c = args[args.length - 1];
    const vals = args.slice(0, args.length - 1);
    const lst = ((rest) => (rest.length === 0)
                 ? null
                 : { car: rest[0], cdr: lst(rest.slice(1)) });
    return __apply_cont(c, lst(vals));
  }
};
const __append = {
  fun: (e, a, b, c) => {
    const app = ((rest) => (rest === null)
                 ? b
                 : { car: rest.car, cdr: app(rest.cdr) });
    return __apply_cont(c, app(a));
  }
};

const __nilQUEST = {
  fun: (e, l, c) => {
    return __apply_cont(c, (l === null));
  }
};
const __eqQUEST = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a == b));
  }
};
const __equalQUEST = {
  fun: (e, a, b, c) => {
    return __apply_cont(c, (a === b));
  }
};
const __not = {
  fun: (e, a, c) => {
    return __apply_cont(c, !a);
  }
};
const __ref = {
  fun: (e, init, c) => {
    return __apply_cont(c, { ref: init });
  }
};
const __assignBANG = {
  fun: (e, r, val, c) => {
    r.ref = val;
    return __apply_cont(c, r);
  }
};
const __deref = {
  fun: (e, r, c) => {
    return __apply_cont(c, r.ref);
  }
};
const __callDIVcurrent_continuation = {
  fun: (e, f, c) => {
    const reified = { fun: (e, ret, _) => __apply_cont(c, ret) };
    return __apply(f, reified, c);
  }
};
const __raise = {
  fun: (e, ex, c) => {
    // FIXME Actually implement this.
    throw ex
  }
};
