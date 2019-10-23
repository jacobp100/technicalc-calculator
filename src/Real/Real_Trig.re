open Real_Types;

let mod2Pi = a =>
  switch (a) {
  | Rational(n, d, Pi) =>
    let divisor = 2 * d;
    let n = (n mod divisor + divisor) mod divisor;
    Rational(n, d, Pi);
  | _ => a
  };

let sin = a =>
  switch (mod2Pi(a)) {
  | Rational(0, 1, _)
  | Rational(1 | 2, 1, Pi) => zero
  | Rational(1, 2, Pi) => one
  | Rational(3, 2, Pi) => minusOne
  | Rational(1 | 2, 3, Pi) => Rational(1, 2, Sqrt(3))
  | Rational(4 | 5, 3, Pi) => Rational(-1, 2, Sqrt(3))
  | Rational(1 | 3, 4, Pi) => Rational(1, 2, Sqrt(2))
  | Rational(5 | 7, 4, Pi) => Rational(-1, 2, Sqrt(2))
  | Rational(1 | 5, 6, Pi) => Rational(1, 2, Unit)
  | Rational(7 | 11, 6, Pi) => Rational(-1, 2, Unit)
  | Rational(n, d, c) => Float(ratFloat(n, d, c)->sin)
  | Float(f) => Float(sin(f))
  };

let cos = a =>
  switch (mod2Pi(a)) {
  | Rational(0, 1, _)
  | Rational(2, 1, Pi) => one
  | Rational(1, 1, Pi) => minusOne
  | Rational(1 | 3, 2, Pi) => zero
  | Rational(1 | 5, 3, Pi) => Rational(1, 2, Unit)
  | Rational(2 | 4, 3, Pi) => Rational(-1, 2, Unit)
  | Rational(1 | 7, 4, Pi) => Rational(1, 2, Sqrt(2))
  | Rational(3 | 5, 4, Pi) => Rational(-1, 2, Sqrt(2))
  | Rational(1 | 11, 6, Pi) => Rational(1, 2, Sqrt(3))
  | Rational(5 | 7, 6, Pi) => Rational(-1, 2, Sqrt(3))
  | Rational(n, d, c) => Float(ratFloat(n, d, c)->cos)
  | Float(f) => Float(cos(f))
  };