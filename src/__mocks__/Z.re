module Bigint = {
  exception Overflow;
  type t;

  [@bs.new] [@bs.module] external of_int: int => t = "bn.js";
  let of_int = of_int;
  [@bs.new] [@bs.module] external of_float: float => t = "bn.js";
  let of_float = of_float;
  [@bs.new] [@bs.module] external of_string: string => t = "bn.js";
  let of_string = of_string;

  let zero = of_int(0);
  let one = of_int(1);
  let minus_one = of_int(-1);

  let _to_float: t => float = [%raw "x => x.toNumber()"];
  let to_float = a =>
    switch (_to_float(a)) {
    | v => v
    | exception _ => raise(Overflow)
    };
  let _to_int: t => int = [%raw "x => (x.toNumber() | 0)"];
  let to_int = a =>
    switch (_to_int(a)) {
    | v => v
    | exception _ => raise(Overflow)
    };
  let to_string: t => string = [%raw "x => x.toString(10)"];

  let add: (t, t) => t = [%raw "(a, b) => a.add(b)"];
  let sub: (t, t) => t = [%raw "(a, b) => a.sub(b)"];
  let mul: (t, t) => t = [%raw "(a, b) => a.mul(b)"];
  let div: (t, t) => t = [%raw "(a, b) => a.div(b)"];
  let rem: (t, t) => t = [%raw "(a, b) => a.mod(b)"];
  let pow: (t, t) => t = [%raw "(a, b) => a.pow(b)"];

  let abs: t => t = [%raw "(a) => a.abs()"];

  let cmp: (t, t) => int = [%raw "(a, b) => a.cmp(b)"];
  let equal = (a, b) => cmp(a, b) == 0;
  let lt = (a, b) => cmp(a, b) == (-1);
  let lte = (a, b) => cmp(a, b) != 1;
  let gt = (a, b) => cmp(a, b) == 1;
  let gte = (a, b) => cmp(a, b) != (-1);
};
