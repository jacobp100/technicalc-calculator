type t;

/* Bypass greatest common divisor */
external _fast_construct: ((Z.t, Z.t)) => t = "%identity";

[@bs.module] external of_ints: (int, int) => t = "big-rat";
[@bs.module] external of_int: int => t = "big-rat";
[@bs.module] external of_float: float => t = "big-rat";
[@bs.module] external make: (Z.t, Z.t) => t = "big-rat";
let of_bigint = n => _fast_construct((n, Z.one));

let one = of_int(1);
let zero = of_int(0);
let minus_one = of_int(-1);

[@bs.get_index] external num: (t, [@bs.as 0] _) => Z.t = "";
[@bs.get_index] external den: (t, [@bs.as 1] _) => Z.t = "";

[@bs.module] external add: (t, t) => t = "big-rat/add";
[@bs.module] external sub: (t, t) => t = "big-rat/sub";
[@bs.module] external mul: (t, t) => t = "big-rat/mul";
[@bs.module] external div: (t, t) => t = "big-rat/div";
[@bs.module] external neg: t => t = "big-rat/neg";
[@bs.module] external abs: t => t = "big-rat/abs";
[@bs.module] external inv: t => t = "big-rat/recip";
[@bs.module] external to_string: t => string = "big-rat/to-string";
[@bs.module] external to_float: t => float = "big-rat/to-float";
[@bs.module] external cmp: (t, t) => int = "big-rat/cmp";
let equal = (a: t, b: t): bool => cmp(a, b) == 0;
let lt = (a, b) => cmp(a, b) == (-1);
let leq = (a, b) => cmp(a, b) != 1;
let gt = (a, b) => cmp(a, b) == 1;
let geq = (a, b) => cmp(a, b) != (-1);
