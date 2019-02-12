let pi = 4.0 *. atan(1.0);
let asinh = f => log(f +. sqrt(f *. f +. 1.0));
let acosh = f => log(f +. sqrt(f *. f -. 1.0));
let atanh = f => log((1.0 +. f) /. (1.0 -. f)) /. 2.0;

let default = (defaultValue, arg) =>
  switch (arg) {
  | Some(v) => v
  | None => defaultValue
  };

let f_is_int = f => floor(f) == f;

let bounds = (~lower=?, ~upper=?, f) => {
  let lowerCompare =
    switch (lower) {
    | Some(l) => compare(l, f)
    | None => (-1)
    };
  let upperCompare =
    switch (upper) {
    | Some(u) => compare(u, f)
    | None => 1
    };
  switch (lowerCompare, upperCompare) {
  | (0, 0) => `BothBound
  | (0, _) => `LowerBound
  | (_, 0) => `UpperBound
  | ((-1), 1) => `Inside(f)
  | _ => `Outside
  };
};

let rec string_split_on_char = (c, v) =>
  switch (String.index(v, c)) {
  | i => [
      String.sub(v, 0, i),
      ...string_split_on_char(
           c,
           String.sub(v, i + 1, String.length(v) - 1 - i),
         ),
    ]
  | exception Not_found => [v]
  };

let q_exp_ints = (base, a) =>
  if (a == 0) {
    Q.one;
  } else if (a > 0) {
    Q.of_bigint(Z.pow(Z.of_int(base), a));
  } else {
    Q.of_bigint(Z.pow(Z.of_int(base), abs(a))) |> Q.inv;
  };

let rec _find_q_magnitude = (base, q, approx) =>
  switch (
    Q.compare(q, q_exp_ints(base, approx)),
    Q.compare(q, q_exp_ints(base, approx + 1)),
  ) {
  | (1 | 0, (-1)) => approx
  | (1, 0) => approx + 1
  | ((-1), _) => _find_q_magnitude(base, q, approx - 1)
  | (_, 1) => _find_q_magnitude(base, q, approx + 1)
  | _ => raise(Not_found)
  };

let q_magnitude_base = (base, x) =>
  if (Q.equal(x, Q.zero)) {
    0;
  } else {
    let abs_x = Q.abs(x);
    let log2_base = log(float_of_int(base)) /. log(2.);
    let approx_log2_q =
      float_of_int(Z.log2(Q.num(abs_x)) - Z.log2(Q.den(abs_x)));
    let approx_magnitude = int_of_float(approx_log2_q /. log2_base);

    let approx = ref(approx_magnitude);
    while (Q.lt(abs_x, q_exp_ints(10, approx^))) {
      approx := approx^ - 1;
    };
    while (Q.geq(abs_x, q_exp_ints(10, approx^ + 1))) {
      approx := approx^ + 1;
    };

    let exact = approx^;
    exact;
  };
let q_magnitude = q_magnitude_base(10);

let q_safe_mod_z = (a, b) => {
  let denominator = Q.den(a);
  let divisor = Z.mul(b, denominator);
  let numerator = Z.rem(Q.num(a), divisor);
  /* Handle negative numerators */
  let numerator = Z.rem(Z.add(numerator, divisor), divisor);
  Q.make(numerator, denominator);
};

let q_is_int = a => Z.equal(Q.den(a), Z.one);

let q_floor = a =>
  Q.num(
    if (q_is_int(a)) {
      a;
    } else {
      let integer_part = q_safe_mod_z(a, Z.one);
      Q.sub(a, integer_part);
    },
  );
