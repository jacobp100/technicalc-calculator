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

let q_exp_10 = a =>
  switch (Z.cmp(a, Z.zero)) {
  | 0 => Q.one
  | 1 => Q.of_bigint(Z.pow(Z.of_int(10), a))
  | (-1) => Q.of_bigint(Z.pow(Z.of_int(10), Z.abs(a))) |> Q.inv
  | _ => raise(Not_found)
  };

let z_magnitude = x =>
  Z.of_int(String.length(Z.to_string(Z.abs(x))) - 1);

let q_magnitude = x => {
  /*
   You *could* do something with exponential search (done previously).
   However, you make so many instances of Z, that this is likely much faster
   */
  let abs_x = Q.abs(x);
  let approx = Z.sub(z_magnitude(Q.num(x)), z_magnitude(Q.den(x)));
  let approx = ref(approx);
  while (Q.lt(abs_x, q_exp_10(approx^))) {
    approx := Z.sub(approx^, Z.one);
  };
  while (Q.gt(abs_x, q_exp_10(Z.add(approx^, Z.one)))) {
    approx := Z.add(approx^, Z.one);
  };
  let exact = approx^;
  exact;
};

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
