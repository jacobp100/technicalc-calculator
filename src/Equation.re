open Complex;

let (==) = equal;
let (+) = add;
let (-) = sub;
let ( * ) = mul;
let (/) = div;
let (~-) = neg;
let ( ** ) = pow;

let (_1, _2, _3, _6, _4, _9, _27) = (
  of_int(1),
  of_int(2),
  of_int(3),
  of_int(6),
  of_int(4),
  of_int(9),
  of_int(27),
);
let epsilon = 1.e-8;
let base = of_int(1000000);
let round_to_precision = x =>
  x * base |> to_float |> int_of_float |> of_int |> div(_, base);

let quadratic = (a, b, c) => {
  let determinant = sqrt(b ** _2 - _4 * a * c);
  let x1 = (- b + determinant) / (_2 * a);
  let x2 = (- b - determinant) / (_2 * a);
  (x1, x2);
};

let _cubic_raphson = (a, b, c, d, start) => {
  let x = ref(start);
  let i = ref(0);
  let maxI = 8;

  while (i^ < maxI) {
    let f'x = _3 * a * x^ ** _2 + _2 * b * x^ + c;

    if (f'x == zero) {
      i := maxI;
    } else {
      let fx = a * x^ ** _3 + b * x^ ** _2 + c * x^ + d;
      let dx = fx / f'x;
      x := x^ - dx;
      if (to_float(abs(dx)) < epsilon) {
        i := maxI;
      } else {
        i := Pervasives.(+)(i^, 1);
      };
    };
  };

  let x0 = round_to_precision(x^);
  let fx = a * x0 ** _3 + b * x0 ** _2 + c * x0 + d;
  if (fx == zero) {
    let (x1, x2) = quadratic(a, a * x0 + b, a * x0 ** _2 + b * x0 + c);
    Some((x0, x1, x2));
  } else {
    None;
  };
};

let _cubic_raphson = (a, b, c, d) => {
  let (m0, m1) = quadratic(_3 * a, _2 * b, c);
  let midpoint = (m0 + m1) / _2;
  let (range_re, range_im) = to_components(m0 - m1);
  let range = of_components(Real.abs(range_re), Real.abs(range_im));
  let values = [midpoint, midpoint - range, midpoint + range];
  List.fold_left(
    (current, value) =>
      switch (current) {
      | None => _cubic_raphson(a, b, c, d, value)
      | v => v
      },
    None,
    values,
  );
};

let _cubic_numeric = (a, b, c, d) => {
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
      ** of_real(Real.of_int(1, ~denominator=3));
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
  switch (_cubic_raphson(a, b, c, d)) {
  | Some(v) => v
  | None => _cubic_numeric(a, b, c, d)
  };
