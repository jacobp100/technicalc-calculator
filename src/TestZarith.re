module ZBigint = {
  exception Overflow;
  type t;

  [@bs.new] [@bs.module] external of_int: int => t = "bn.js";
  let of_int = of_int;

  let zero = of_int(0);
  let one = of_int(1);
  let minus_one = of_int(-1);

  let to_float = (x: t): float => [%raw "x.toNumber()"];
  let to_int = (x: t): int => [%raw "(x.toInt() | 0)"];
  let to_string = (x: t): string => [%raw "x.toString()"];

  let add = (a: t, b: t): t => [%raw "a.add(b)"];
  let sub = (a: t, b: t): t => [%raw "a.sub(b)"];
  let mul = (a: t, b: t): t => [%raw "a.mul(b)"];
  let div = (a: t, b: t): t => [%raw "a.div(b)"];
  let rem = (a: t, b: t): t => [%raw "a.mod(b)"];
};

module QBigint = {
  type t;

  [@bs.module] external make: (ZBigint.t, ZBigint.t) => t = "big-rat";
  let make = make;
  [@bs.module] external of_ints: (int, int) => t = "big-rat";
  let of_ints = of_ints;
  [@bs.module] external of_int: int => t = "big-rat";
  let of_int = of_int;
  [@bs.module] external of_float: float => t = "big-rat";
  let of_float = of_float;

  let one = of_int(1);
  let zero = of_int(0);
  let minus_one = of_int(-1);

  let num = (x: t): ZBigint.t => [%raw "x => x[0]"];
  let den = (x: t): ZBigint.t => [%raw "x => x[1]"];

  [@bs.module] external add: (t, t) => t = "big-rat/add";
  let add = add;

  [@bs.module] external sub: (t, t) => t = "big-rat/sub";
  let sub = sub;

  [@bs.module] external mul: (t, t) => t = "big-rat/mul";
  let mul = mul;

  [@bs.module] external div: (t, t) => t = "big-rat/div";
  let div = div;

  [@bs.module] external neg: t => t = "big-rat/neg";
  let neg = neg;

  [@bs.module] external to_string: t => string = "big-rat/to-string";
  let to_string = to_string;

  [@bs.module] external to_float: t => float = "big-rat/to-float";
  let to_float = to_float;
};
