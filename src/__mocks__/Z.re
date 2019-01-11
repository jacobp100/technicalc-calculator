module Bigint = {
  exception Overflow;
  type t;

  [@bs.new] [@bs.module] external of_int: int => t = "bn.js";
  let of_int = of_int;

  let zero = of_int(0);
  let one = of_int(1);
  let minus_one = of_int(-1);

  let to_float: t => float = [%raw "x => x.toNumber()"];
  let to_int: t => int = [%raw "x => (x.toNumber() | 0)"];
  let to_string: t => string = [%raw "x => x.toString()"];

  let add: (t, t) => t = [%raw "(a, b) => a.add(b)"];
  let sub: (t, t) => t = [%raw "(a, b) => a.sub(b)"];
  let mul: (t, t) => t = [%raw "(a, b) => a.mul(b)"];
  let div: (t, t) => t = [%raw "(a, b) => a.div(b)"];
  let rem: (t, t) => t = [%raw "(a, b) => a.mod(b)"];
  let pow: (t, t) => t = [%raw "(a, b) => a.pow(b)"];

  let cmp: (t, t) => int = [%raw "(a, b) => a.cmp(b)"];
  let equal = (a, b) => cmp(a, b) == 0;
  let lt = (a, b) => cmp(a, b) == (-1);
};
