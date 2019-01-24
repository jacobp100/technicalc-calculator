module Zt = Z.Bigint;
module Qt = Q.Bigint;

let default = (defaultValue, arg) =>
  switch (arg) {
  | Some(v) => v
  | None => defaultValue
  };

type boundary =
  | BothBound
  | UpperBound
  | LowerBound
  | Inside(float)
  | Outside;

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
  | (0, 0) => BothBound
  | (0, _) => LowerBound
  | (_, 0) => UpperBound
  | ((-1), 1) => Inside(f)
  | _ => Outside
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
  switch (Zt.cmp(a, Zt.zero)) {
  | 0 => Qt.one
  | 1 => Qt.of_bigint(Zt.pow(Zt.of_int(10), a))
  | (-1) => Qt.of_bigint(Zt.pow(Zt.of_int(10), Zt.abs(a))) |> Qt.inv
  | _ => raise(Not_found)
  };

let z_magnitude = x =>
  Zt.of_int(String.length(Zt.to_string(Zt.abs(x))) - 1);

let q_magnitude = x => {
  /*
   You *could* do something with exponential search (done previously).
   However, you make so many instances of Zt, that this is likely much faster
   */
  let abs_x = Qt.abs(x);
  let approx = Zt.sub(z_magnitude(Qt.num(x)), z_magnitude(Qt.den(x)));
  let approx = ref(approx);
  while (Qt.lt(abs_x, q_exp_10(approx^))) {
    approx := Zt.sub(approx^, Zt.one);
  };
  while (Qt.gt(abs_x, q_exp_10(Zt.add(approx^, Zt.one)))) {
    approx := Zt.add(approx^, Zt.one);
  };
  let exact = approx^;
  exact;
};

let q_safe_mod_z = (a, b) => {
  let denominator = Qt.den(a);
  let divisor = Zt.mul(b, denominator);
  let numerator = Zt.rem(Qt.num(a), divisor);
  /* Handle negative numerators */
  let numerator = Zt.rem(Zt.add(numerator, divisor), divisor);
  Qt.make(numerator, denominator);
};

let q_floor = a => {
  let integer_part = q_safe_mod_z(a, Zt.one);
  let floored = Qt.sub(a, integer_part);

  if (!Zt.equal(Qt.den(floored), Zt.one)) {
    raise(Not_found);
  };

  Qt.num(floored);
};
