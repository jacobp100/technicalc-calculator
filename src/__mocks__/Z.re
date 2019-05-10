exception Overflow;
type t;

[@bs.new] [@bs.module "bn.js"] external of_int: int => t = "default";
[@bs.new] [@bs.module "bn.js"] external of_float: float => t = "default";
[@bs.new] [@bs.module "bn.js"]
external _of_string_base: (string, int) => t = "default";
let _trim_leading_plus = Js.String.replaceByRe([%re "/$\\+/"], "");
let of_string_base = (base, s) =>
  _of_string_base(_trim_leading_plus(s), base);
let of_string = s => _of_string_base(_trim_leading_plus(s), 10);

let zero = of_int(0);
let one = of_int(1);
let minus_one = of_int(-1);

[@bs.send] external _to_float: t => float = "toNumber";
let to_float = a =>
  switch (_to_float(a)) {
  | v => v
  | exception _ => infinity
  };
let to_int = a =>
  switch (int_of_float(_to_float(a))) {
  | v => v
  | exception _ => raise(Overflow)
  };
[@bs.send.pipe: t] external to_string_base: int => string = "toString";
[@bs.send] external to_string: (t, [@bs.as 10] _) => string = "toString";

[@bs.send] external add: (t, t) => t = "add";
[@bs.send] external sub: (t, t) => t = "sub";
[@bs.send] external mul: (t, t) => t = "mul";
[@bs.send] external div: (t, t) => t = "div";
[@bs.send] external rem: (t, t) => t = "mod";
[@bs.send] external _pow_z: (t, t) => t = "pow";
let pow = (a, b) => _pow_z(a, of_int(b));

[@bs.send] external abs: t => t = "abs";
[@bs.send] external neg: t => t = "neg";

[@bs.send] external compare: (t, t) => int = "cmp";
[@bs.send] external equal: (t, t) => bool = "eq";
[@bs.send] external lt: (t, t) => bool = "lt";
[@bs.send] external leq: (t, t) => bool = "lte";
[@bs.send] external gt: (t, t) => bool = "gt";
[@bs.send] external geq: (t, t) => bool = "gte";

[@bs.send] external pred: (t, [@bs.as 1] _) => t = "subn";
[@bs.send] external succ: (t, [@bs.as 1] _) => t = "addn";
[@bs.send] external sign: (t, [@bs.as 0] _) => int = "cmpn";
[@bs.send] external numbits: t => int = "bitLength";

let log2 = x =>
  if (sign(x) > 0) {
    numbits(x) - 1;
  } else {
    invalid_arg("Z.log2");
  };
let log2up = x =>
  if (sign(x) > 0) {
    numbits(pred(x));
  } else {
    invalid_arg("Z.log2up");
  };

let (!=) = (a, b) => !equal(a, b);
let (==) = equal;
let (+) = add;
let (-) = sub;
let ( * ) = mul;
let (/) = div;
/* let (mod) = rem; */
let (~-) = neg;
let (<) = lt;
let (<=) = leq;
let (>) = gt;
let (>=) = geq;
