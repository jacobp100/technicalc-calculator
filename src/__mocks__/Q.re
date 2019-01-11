module Bigint = {
  type t;

  [@bs.module] external make: (Z.Bigint.t, Z.Bigint.t) => t = "big-rat";
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

  let num: t => Z.Bigint.t = [%raw "x => x[0]"];
  let den: t => Z.Bigint.t = [%raw "x => x[1]"];

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

  [@bs.module] external cmp: (t, t) => int = "big-rat/cmp";

  let equal = (a: t, b: t): bool => cmp(a, b) == 0;
};
