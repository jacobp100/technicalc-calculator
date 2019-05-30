open Types;
open Base;

let ( **. ) = Pervasives.( ** );

let (_1, _2, _3, _6, _4, _9, _27) = (
  ofInt(1),
  ofInt(2),
  ofInt(3),
  ofInt(6),
  ofInt(4),
  ofInt(9),
  ofInt(27),
);
let epsilon = 1.e-4;
let base = 27720; /* Divisible by all numbers 1-12 */
let roundToPrecision = x =>
  int_of_float(x *. float_of_int(base))->ofInt / ofInt(base);

let quadratic = (a, b, c) => {
  let determinant = sqrt(b ** _2 - _4 * a * c);
  let x1 = (- b + determinant) / (_2 * a);
  let x2 = (- b - determinant) / (_2 * a);
  (x1, x2);
};

let _cubicRaphson = (a, b, c, d) => {
  /*
   Attempt to find an exact real root within an amount of decimal places
   Some roots may be complex, or be of square roots, which this method will not find
   As long as we find one exact root, we can factor the equation to a quadratic, which
   will find roots that could be complex or formed of square roots
   We search three areas to find roots: before, inbetween, and after the two maxima of
   the cubic
   */
  let (m0, m1) = quadratic(_3 * a, _2 * b, c);
  let m0 = toFloat(m0);
  let m1 = toFloat(m1);

  let midpoint = (m0 +. m1) /. 2.;
  let range = abs_float(m0 -. m1);
  let values = [midpoint, midpoint -. range, midpoint +. range];

  let af = toFloat(a);
  let bf = toFloat(b);
  let cf = toFloat(c);
  let df = toFloat(d);

  values
  ->Belt.List.reduce(None, (current, value) =>
      switch (current) {
      | None =>
        let x0f = Raphson.cubic(af, bf, cf, df, value);
        let x0 = roundToPrecision(x0f);
        let fx = a * x0 ** _3 + b * x0 ** _2 + c * x0 + d;
        if (FloatUtil.isFinite(x0f) && fx == zero) {
          let (x1, x2) = quadratic(a, a * x0 + b, a * x0 ** _2 + b * x0 + c);
          Some((x0, x1, x2));
        } else {
          None;
        };
      | v => v
      }
    );
};

let _cubicNumeric = (a, b, c, d) => {
  /* See https://math.stackexchange.com/questions/61725/is-there-a-systematic-way-of-solving-cubic-equations */
  let q =
    sqrt(
      (_2 * b ** _3 - _9 * a * b * c + _27 * a ** _2 * d)
      ** _2
      - _4
      * (b * b - _3 * a * c)
      ** _3,
    );
  let determinant = b ** _2 - _3 * a * c;

  if (q == zero && determinant == zero) {
    let x1 = - b / (_3 * a);
    (x1, x1, x1);
  } else {
    let c0 =
      ((q + _2 * b ** _3 - _9 * a * b * c + _27 * a ** _2 * d) / _2)
      ** real(Q.of_ints(1, 3));
    let x1 =
      - b / (_3 * a) - c0 / (_3 * a) - (b ** _2 - _3 * a * c) / (_3 * a * c0);
    let x2 =
      - b
      / (_3 * a)
      + c0
      * _1
      / (_6 * a)
      + c0
      * sqrt(- _3)
      / (_6 * a)
      + determinant
      / (_6 * a * c0)
      - sqrt(- _3)
      * determinant
      / (_6 * a * c0);
    let x3 =
      - b
      / (_3 * a)
      + c0
      * (_1 - sqrt(- _3))
      / (_6 * a)
      + (_1 + sqrt(- _3))
      * determinant
      / (_6 * a * c0);
    (x1, x2, x3);
  };
};

let cubic = (a, b, c, d) =>
  switch (_cubicRaphson(a, b, c, d)) {
  | Some(v) => v
  | None => _cubicNumeric(a, b, c, d)
  };
