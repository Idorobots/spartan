const toSafeName = (name) => {
  // TODO Handle unicode characters.
  return "__" + name
    .replace("*", "MULT")
    .replace("+", "PLUS")
    .replace("_", "UNDER")
    .replace("-", "_")
    .replace("/", "DIV")
    .replace("\\", "BACK")
    .replace("|", "BAR")
    .replace("!", "BANG")
    .replace("?", "QUEST")
    .replace("=", "EQUAL")
    .replace("<", "LESS")
    .replace(">", "GREATER")
    .replace("%", "PROC")
    .replace("^", "CARET")
    .replace("&", "AMPER")
    .replace("@", "AT")
    .replace("$", "DOLLAR")
    .replace("~", "TYLDE")
}

const importModule = (init) => {
  const mod = trampoline(init());
  for (name in mod) {
    globalThis[toSafeName(name)] = mod[name];
  }
}

let __kontCounter = __rt_continuation_hops;

importModule(__core);

__rt_start.fun(
  __rt_start.env,
  makeClosure(null, (e, c) => __module_init()),
  makeClosure(null, (e, v) => v)
);
