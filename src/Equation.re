open Complex;

let (==) = equal;
let (+) = add;
let (-) = sub;
let ( * ) = mul;
let (/) = div;
let (~-) = neg;
let ( ** ) = pow;

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
let base = 1000;
let roundToPrecision = x =>
  (x *. float_of_int(base))->int_of_float->ofInt->div(_, ofInt(base));

let var2 = (x0, y0, c0, x1, y1, c1) => {
  let denom = x0 * y1 - y0 * x1;
  ((y1 * c0 - y0 * c1) / denom, (x0 * c1 - x1 * c0) / denom);
};

let var3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) => {
  // https://www.wolframalpha.com/input/?i=inverse(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D)+*+%7B%7Bj%7D,%7Bk%7D,%7Bl%7D%7D
  // Literally the worst formatting
  let denom =
    - z0
    * y1
    * x2
    + y0
    * z1
    * x2
    + z0
    * x1
    * y2
    - x0
    * z1
    * y2
    - y0
    * x1
    * z2
    + x0
    * y1
    * z2;
  let p1 =
    (y1 * z2 - z1 * y2)
    * c0
    + (z0 * y2 - y0 * z2)
    * c1
    + (y0 * z1 - z0 * y1)
    * c2;
  let p2 =
    (z1 * x2 - x1 * z2)
    * c0
    + (x0 * z2 - z0 * x2)
    * c1
    + (z0 * x1 - x0 * z1)
    * c2;
  let p3 =
    (x1 * y2 - y1 * x2)
    * c0
    + (y0 * x2 - x0 * y2)
    * c1
    + (x0 * y1 - y0 * x1)
    * c2;
  (p1 / denom, p2 / denom, p3 / denom);
};

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
  switch (toFloats(m0), toFloats(m1)) {
  | ((m0, 0.), (m1, 0.)) =>
    let midpoint = (m0 +. m1) /. 2.;
    let range = abs_float(m0 -. m1);
    let values = [midpoint, midpoint -. range, midpoint +. range];

    let af = toFloat(a);
    let bf = toFloat(b);
    let cf = toFloat(c);
    let df = toFloat(d);

    values->Belt.List.reduce(None, (current, value) =>
      switch (current) {
      | None =>
        let x0 = Raphson.cubic(af, bf, cf, df, value)->roundToPrecision;
        let fx = a * x0 ** _3 + b * x0 ** _2 + c * x0 + d;
        if (fx == zero) {
          let (x1, x2) = quadratic(a, a * x0 + b, a * x0 ** _2 + b * x0 + c);
          Some((x0, x1, x2));
        } else {
          None;
        };
      | v => v
      }
    );
  | _ => None
  };
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
      ** ofReal(Real.ofInt(1, ~denominator=3));
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
