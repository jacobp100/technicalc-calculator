type t =
  | Rational(int, int, Real_Constant.t)
  | Decimal(Decimal.t);

let ratDecimal = (n, d, c) =>
  Decimal.(ofInt(n) / ofInt(d) * Real_Constant.toDecimal(c));

let rec gcd = (a, b) => b == 0 ? a : gcd(b, a mod b);

let one = Rational(1, 1, Unit);
let minusOne = Rational(-1, 1, Unit);
let zero = Rational(0, 1, Unit);
let nan = Rational(1, 0, Unit);
let isNaN = a =>
  switch (a) {
  | Rational(_, 0, _) => true
  | Decimal(f) => !Decimal.isFinite(f)
  | _ => false
  };

let ofInt = n =>
  if (n->float_of_int->int_of_float == n) {
    Rational(n, 1, Unit);
  } else {
    failwith("Not an integer");
  };

let decimal = f => Decimal(f);

let rational = (n, d, c) =>
  if (d == 0) {
    nan;
  } else {
    let n = d >= 0 ? n : - n;
    let d = abs(d);
    let gcd = gcd(abs(n), d);
    let n = n / gcd;
    let d = d / gcd;

    switch (Real_Constant.simplify(c)) {
    | `Zero => Rational(0, 1, Unit)
    | `None => Rational(n, d, c)
    | `Factor(n', c) =>
      switch (SafeInt.((ofInt(n) * ofInt(n'))->toInt)) {
      | Some(n) => Rational(n, d, c)
      | None => Decimal(ratDecimal(d, d, c))
      }
    };
  };

let toDecimal = a =>
  switch (a) {
  | Rational(n, d, c) => ratDecimal(n, d, c)
  | Decimal(d) => d
  };