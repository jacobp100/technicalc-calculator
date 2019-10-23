type t =
  | Rational(int, int, Real_Constant.t)
  | Float(float);

let ratFloat = (n, d, c) =>
  float_of_int(n) /. float_of_int(d) *. Real_Constant.toFloat(c);

let rec gcd = (a, b) => b == 0 ? a : gcd(b, a mod b);

let one = Rational(1, 1, Unit);
let minusOne = Rational(-1, 1, Unit);
let zero = Rational(0, 1, Unit);
let nan = Rational(1, 0, Unit);
let equal = (a, b) => a == b;
let isNaN = _ => false;

let fromInt = n => Rational(n, 1, Unit);

let rat = (n, d, c) =>
  if (d == 0) {
    nan;
  } else {
    let gcd = gcd(n, d);
    let n = n / gcd;
    let d = d / gcd;

    switch (Real_Constant.simplify(c)) {
    | `Zero => Rational(0, 1, Unit)
    | `None => Rational(n, d, c)
    | `Factor(n', c) =>
      switch (SafeInt.((fromInt(n) * fromInt(n'))->toInt)) {
      | Some(n) => Rational(n, d, c)
      | None => Float(ratFloat(d, d, c))
      }
    };
  };

let toFloat = a =>
  switch (a) {
  | Rational(n, d, c) => ratFloat(n, d, c)
  | Float(f) => f
  };

let (==) = equal;