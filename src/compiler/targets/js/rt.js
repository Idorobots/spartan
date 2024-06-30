let __kontCounter = __rt_continuation_hops;

let cc = __module_init();

while(cc !== null && typeof cc === "object" && typeof cc.kont === "object") {
  cc = cc.kont.fun(cc.kont.env, cc.hole)
}
