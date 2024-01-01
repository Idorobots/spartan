// Internal procedures
function __write(o) {
  switch(typeof o) {
  case "string":
    process.stdout.write(o);
    break;
  case "object":
    if (o === __nil) {
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

      while (i !== __nil) {
        __write(i.car);

        if(i.cdr === __nil) {
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
const __display = (v, c) => {
  __write(v);
  return c(__nil);
};

const __newline = (c) => {
  __write("\n");
  return c(__nil);
};

const __PLUS = (a, b, c) => {
  return c((a + b));
};

const ___ = (a, b, c) => {
  return c((a - b));
};

const __MULT = (a, b, c) => {
  return c((a * b));
};

const __DIV = (a, b, c) => {
  return c((a / b));
};

const __modulo = (a, b, c) => {
  return c((a % b));
};

const __quotient = (a, b, c) => {
  return c(Math.floor(a/b));
};

const __LESS = (a, b, c) => {
  return c((a < b));
};

const __LESSEQUAL = (a, b, c) => {
  return c((a <= b));
};

const __GREATER = (a, b, c) => {
  return c((a > b));
};

const __GREATEREQUAL = (a, b, c) => {
  return c((a >= b));
};

const __EQUAL = (a, b, c) => {
  return c((a == b));
};

const __cons = (a, b, c) => {
  return c({ car: a, cdr: b });
};

const __car = (l, c) => {
  return c(l.car);
};

const __cdr = (l, c) => {
  return c(l.cdr);
};

const __list = (...args) => {
  const c = args[args.length - 1];
  const vals = args.slice(0, args.length - 1);
  const lst = ((rest) => (rest.length === 0)
               ? __nil
               : { car: rest[0], cdr: lst(rest.slice(1)) });
  return c(lst(vals));
};

const __append = (a, b, c) => {
  const app = ((rest) => (rest === __nil)
               ? b
               : { car: rest.car, cdr: app(rest.cdr) });
  return c(app(a));
};

const __nilQUEST = (l, c) => {
  return c((l === __nil));
};

const __eqQUEST = (a, b, c) => {
  return c((a == b));
};

const __equalQUEST = (a, b, c) => {
  return c((a === b));
};

const __not = (a, c) => {
  return c(!a);
};

const __ref = (init, c) => {
  return c({ ref: init });
};

const __assignBANG = (r, val, c) => {
  r.ref = val;
  return c(r);
};

const __deref = (r, c) => {
  return c(r.ref);
};

const __callDIVcurrent_continuation = (f, c) => {
  const reified = (ret, _) => c(ret);
  return f(reified, c);
};

const __raise = (ex, c) => {
  // FIXME Actually implement this.
  throw ex
};
