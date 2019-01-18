module Zt = Z.Bigint;
module Qt = Q.Bigint;

let (==) = Qt.equal;
let (+) = Qt.add;
/* let (-) = Qt.sub; */
let ( * ) = Qt.mul;
let (/) = Qt.div;
/* let (mod) = Qt.rem; */
let (~-) = Qt.neg;
let (<) = Qt.lt;
let (<=) = Qt.lte;
let (>) = Qt.gt;
let (>=) = Qt.gte;
let (==%) = Pervasives.(==);
let (>%) = Pervasives.(>);
let (<%) = Pervasives.(<);
let (>=%) = Pervasives.(>=);
let (<=%) = Pervasives.(<=);

let _float_is_int = a => Pervasives.(==)(floor(a), a);

type t =
  | Value(Qt.t, Constant.t)
  | NaN;

let nan = NaN;
let zero = Value(Qt.zero, None);
let one = Value(Qt.one, None);
let minus_one = Value(Qt.minus_one, None);
let pi = Value(Qt.one, Pi);
let e = Value(Qt.one, Exp(Zt.one));

let is_nan = Pervasives.(==)(NaN);

let to_float = a =>
  switch (a) {
  | Value(aq, ac) => Qt.to_float(aq) *. Constant.to_float(ac)
  | NaN => infinity
  };

let to_int = a =>
  switch (a) {
  | Value(ar, None) when Zt.equal(Qt.den(ar), Zt.one) =>
    switch (Zt.to_int(Qt.num(ar))) {
    | v => Some(v)
    | exception Zt.Overflow => None
    }
  | _ => None
  };

let normalize = a =>
  /* Note redundant branches are for optimisation */
  switch (a) {
  | Value(_, None) => a
  | Value(aq, _) when aq == Qt.zero => Value(Qt.zero, None)
  | Value(_, Pi) => a
  | Value(aq, ac) =>
    let (multiplier, constant) = Constant.simplify(ac);
    Value(aq * Qt.make(multiplier, Zt.one), constant);
  | _ => a
  };

let of_int = (~denominator=1, ~constant=Constant.none, numerator) =>
  normalize(Value(Qt.of_ints(numerator, denominator), constant));

let of_float = (~constant=Constant.none, v) => {
  switch (classify_float(v)) {
  | FP_normal
  | FP_subnormal =>
    let magnitude = 1.e6;
    let int_max_f = float_of_int(max_int);
    let numerator_f = v *. magnitude;
    if (Pervasives.(<)(abs_float(numerator_f), int_max_f)
        && _float_is_int(numerator_f)) {
      let numerator = int_of_float(numerator_f);
      let denominator = int_of_float(magnitude);
      normalize(of_int(numerator, ~denominator, ~constant));
    } else {
      normalize(Value(Qt.of_float(v), constant));
    };
  | FP_zero => zero
  | FP_infinite
  | FP_nan => NaN
  };
};

let of_q = (~constant=Constant.none, v) => normalize(Value(v, constant));

let of_z = (~constant=Constant.none, v) =>
  of_q(~constant, Qt.make(v, Zt.one));

let to_string = (~format=OutputFormat.default, a) => {
  let value = to_float(a);
  let abs_value = abs_float(value);
  let value_magnitude = value ==% 0. ? 0. : floor(log10(abs_value));

  switch (format.style, a) {
  | (Natural, Value(ar, constant))
      when Zt.lt(Qt.den(ar), Zt.of_int(1000000)) && abs_value <% 1e8 =>
    let num = Qt.num(ar);
    let den = Qt.den(ar);

    let showConstant = !Constant.equal(constant, None);
    let showNumerator = !Zt.equal(num, Zt.one) || !showConstant;
    let showDenominator = !Zt.equal(den, Zt.one);

    let minus = !showNumerator && Qt.lt(ar, Qt.zero) ? "-" : "";
    let constant = showConstant ? Constant.to_string(~format, constant) : "";
    let numerator =
      showNumerator ?
        NumberFormat.add_digit_separators(Zt.to_string(num)) : "";
    let denominator = NumberFormat.add_digit_separators(Zt.to_string(den));

    switch (format.mode, showDenominator) {
    | (_, false) => minus ++ numerator ++ constant
    | (String, true) => minus ++ numerator ++ constant ++ "/" ++ denominator
    | (Latex, true) =>
      minus ++ "\\frac{" ++ numerator ++ constant ++ "}{" ++ denominator ++ "}"
    };
  | (Natural | Decimal, Value(_)) =>
    let inside_magnitude_threshold =
      value_magnitude
      >=% format.decimal_min_magnitude
      && value_magnitude
      <=% format.decimal_max_magnitude;

    if (inside_magnitude_threshold) {
      NumberFormat.format_decimal(
        NumberFormat.create_format(
          ~max_decimal_places=format.precision,
          ~digit_separators=value_magnitude >=% 5.,
          (),
        ),
        value,
      );
    } else {
      NumberFormat.format_exponential(
        ~exponent=value_magnitude,
        NumberFormat.create_format(~max_decimal_places=format.precision, ()),
        value,
      );
    };
  | (Scientific, Value(_)) =>
    let exponent = floor(ceil(value_magnitude) /. 3.) *. 3.;
    NumberFormat.format_exponential(
      ~exponent,
      NumberFormat.create_format(
        ~min_decimal_places=format.precision,
        ~max_decimal_places=format.precision,
        (),
      ),
      value,
    );
  | (_, NaN) => "NaN"
  };
};

let q_is_integer = a => Zt.equal(Qt.den(a), Zt.one);
let is_integer = a =>
  switch (a) {
  | Value(ar, None) => q_is_integer(ar)
  | _ => false
  };

let equal = (a, b) =>
  switch (a, b) {
  | (Value(aq, ac), Value(bq, bc)) => aq == bq && Constant.equal(ac, bc)
  | _ => false
  };

let neg = a =>
  switch (a) {
  | Value(ar, ac) => Value(- ar, ac)
  | NaN => a
  };

let add = (a, b) =>
  switch (a, b) {
  | (Value(aq, _), Value(_)) when aq == Qt.zero => b
  | (Value(_), Value(bq, _)) when bq == Qt.zero => a
  | (Value(aq, ac), Value(bq, bc)) when Constant.equal(ac, bc) =>
    of_q(aq + bq, ~constant=ac)
  | (Value(_), Value(_)) => of_float(to_float(a) +. to_float(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let sub = (a, b) => add(a, neg(b));

let mul = (a, b) =>
  switch (a, b) {
  | (Value(aq, _), Value(_)) when aq == Qt.zero => zero
  | (Value(_), Value(bq, _)) when bq == Qt.zero => zero
  | (Value(aq, constant), Value(bq, None))
  | (Value(aq, None), Value(bq, constant)) => of_q(aq * bq, ~constant)
  | (Value(aq, Sqrt(ac)), Value(bq, Sqrt(bc))) =>
    /* Sqrt is simplifed in of_q */
    of_q(aq * bq, ~constant=Sqrt(Zt.mul(ac, bc)))
  | (Value(_), Value(_)) => of_float(to_float(a) *. to_float(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let div = (a, b) =>
  switch (a, b) {
  | (_, Value(value, _)) when value == Qt.zero => NaN
  | (Value(aq, ac), Value(bq, None)) => of_q(aq / bq, ~constant=ac)
  | (Value(aq, ac), Value(bq, bc)) when Constant.equal(ac, bc) =>
    of_q(aq / bq)
  | (Value(aq, Sqrt(ac)), Value(bq, Sqrt(bc)))
      when Zt.gt(ac, bc) && Zt.equal(Zt.rem(ac, bc), Zt.zero) =>
    of_q(aq / bq, ~constant=Sqrt(Zt.div(ac, bc)))
  | (Value(_, _), Value(_, _)) => of_float(to_float(a) /. to_float(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let sqrt = a =>
  switch (a) {
  | Value(aq, _) when aq < Qt.zero => NaN
  | Value(aq, None) when Zt.equal(Qt.den(aq), Zt.one) =>
    of_q(Qt.one, ~constant=Sqrt(Qt.num(aq)))
  | Value(_) => of_float(sqrt(to_float(a)))
  | NaN => NaN
  };

let exp = a =>
  switch (a) {
  | Value(aq, None) when Zt.equal(Qt.den(aq), Zt.one) =>
    of_q(Qt.one, ~constant=Exp(Qt.num(aq)))
  | Value(_) => of_float(exp(to_float(a)))
  | NaN => NaN
  };

type trig_period =
  | FractionOfPi(int, int)
  | NoFraction
  | ValueNaN;

let _trig_period = a =>
  switch (a) {
  | Value(aq, _) when aq == Qt.zero => FractionOfPi(0, 1)
  | Value(aq, Pi) =>
    let denominator = Qt.den(aq);
    let divisor = Zt.mul(Zt.of_int(2), denominator);
    let numerator = Zt.rem(Qt.num(aq), divisor);
    /* Handle negative numerators */
    let numerator = Zt.rem(Zt.add(numerator, divisor), divisor);
    switch (Zt.to_int(numerator), Zt.to_int(denominator)) {
    | (n, d) => FractionOfPi(n, d)
    | exception Zt.Overflow => NoFraction
    };
  | Value(_, _) => NoFraction
  | NaN => ValueNaN
  };

let sin = a =>
  switch (_trig_period(a)) {
  | FractionOfPi(0 | 1 | 2, 1) => of_q(Qt.zero)
  | FractionOfPi(1 | 5, 2) => of_q(Qt.one)
  | FractionOfPi(3 | 7, 2) => of_q(Qt.minus_one)
  | FractionOfPi(1 | 2, 3) =>
    of_int(1, ~denominator=2, ~constant=Sqrt(Zt.of_int(3)))
  | FractionOfPi(4 | 5, 3) =>
    of_int(-1, ~denominator=2, ~constant=Sqrt(Zt.of_int(3)))
  | FractionOfPi(1 | 3, 4) =>
    of_int(1, ~denominator=2, ~constant=Sqrt(Zt.of_int(2)))
  | FractionOfPi(5 | 7, 4) =>
    of_int(-1, ~denominator=2, ~constant=Sqrt(Zt.of_int(2)))
  | FractionOfPi(1 | 5, 6) => of_int(1, ~denominator=2)
  | FractionOfPi(7 | 11, 6) => of_int(-1, ~denominator=2)
  | FractionOfPi(_, _)
  | NoFraction => of_float(sin(to_float(a)))
  | ValueNaN => NaN
  };

let cos = a => sin(add(a, of_int(1, ~denominator=2, ~constant=Pi)));

let tan = a =>
  switch (_trig_period(a)) {
  | FractionOfPi(0 | 1 | 2, 1) => of_q(Qt.zero)
  | FractionOfPi(1 | 5, 4) => of_q(Qt.one)
  | FractionOfPi(3 | 7, 4) => of_q(Qt.minus_one)
  | FractionOfPi(1 | 4, 3) => of_q(Qt.one, ~constant=Sqrt(Zt.of_int(3)))
  | FractionOfPi(2 | 5, 3) =>
    of_q(Qt.minus_one, ~constant=Sqrt(Zt.of_int(3)))
  | FractionOfPi(1 | 7, 6) =>
    of_int(1, ~denominator=3, ~constant=Sqrt(Zt.of_int(3)))
  | FractionOfPi(5 | 11, 6) =>
    of_int(-1, ~denominator=3, ~constant=Sqrt(Zt.of_int(3)))
  | FractionOfPi(1 | 3, 2) => NaN
  | FractionOfPi(_, _)
  | NoFraction => of_float(tan(to_float(a)))
  | ValueNaN => NaN
  };

let pow = (a, b) =>
  switch (a, b) {
  | (Value(ar, _), Value(br, _)) when ar == Qt.zero && br == Qt.zero => NaN
  | (Value(ar, _), Value(_)) when ar == Qt.zero => zero
  | (Value(ar, None), Value(_)) when ar == Qt.one => one
  | (Value(_), Value(br, None)) when br == Qt.of_ints(1, 2) => sqrt(a)
  | (Value(_), Value(br, None)) when br == Qt.of_int(2) => mul(a, a)
  | (Value(ar, Exp(ac)), _) when ar == Qt.one && Zt.equal(ac, Zt.one) =>
    exp(b)
  | (Value(ar, None), Value(br, None))
      when q_is_integer(br) && br > Qt.zero =>
    let (an, ad, bn) = (Qt.num(ar), Qt.den(ar), Qt.num(br));
    of_q(Qt.make(Zt.pow(an, bn), Zt.pow(ad, bn)));
  | (Value(_), Value(_)) => of_float(to_float(a) ** to_float(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let log = a =>
  switch (a) {
  | Value(ar, _) when ar <= Qt.zero => NaN
  | Value(ar, Exp(ac)) when ar == Qt.one => of_z(ac)
  | Value(_) => of_float(log(to_float(a)))
  | NaN => NaN
  };

let abs = a =>
  switch (a) {
  | Value(ar, ac) => of_q(Qt.abs(ar), ~constant=ac)
  | _ => NaN
  };

let factorial = x =>
  switch (to_int(x)) {
  | Some(i) when Pervasives.(<)(i, 100) =>
    let fact = ref(Zt.one);
    for (mul in 2 to i) {
      fact := Zt.mul(fact^, Zt.of_int(mul));
    };
    of_z(fact^);
  | _ => NaN
  };

let _map_float = (fn, x) =>
  switch (x) {
  | Value(_) => of_float(fn(to_float(x)))
  | NaN => NaN
  };

let _check_bounds = (~lower=?, ~upper=?, x) =>
  switch (x) {
  | Value(_) => Util.bounds(~lower?, ~upper?, to_float(x))
  | NaN => Outside
  };

let arcsin = x =>
  switch (_check_bounds(~lower=-1.0, ~upper=1.0, x)) {
  | LowerBound => of_int(-1, ~denominator=2, ~constant=Pi)
  | UpperBound => of_int(1, ~denominator=2, ~constant=Pi)
  | Inside(f) => of_float(asin(f))
  | _ => NaN
  };
let sinh = _map_float(sinh);
let arcsinh = x => {
  let f = to_float(x);
  of_float(Pervasives.log(f +. Pervasives.sqrt(f *. f +. 1.0)));
};
let arccos = x =>
  switch (_check_bounds(~lower=-1.0, ~upper=1.0, x)) {
  | LowerBound => of_int(1, ~constant=Pi)
  | UpperBound => zero
  | Inside(f) => of_float(acos(f))
  | _ => NaN
  };
let cosh = _map_float(cosh);
let arccosh = x =>
  switch (_check_bounds(~lower=1.0, x)) {
  | Inside(f) =>
    of_float(Pervasives.log(f +. Pervasives.sqrt(f *. f -. 1.0)))
  | LowerBound => zero
  | _ => NaN
  };
let arctan = _map_float(atan);
let tanh = _map_float(tanh);
let arctanh = x =>
  switch (_check_bounds(~lower=-1.0, ~upper=1.0, x)) {
  | Inside(f) => of_float(Pervasives.log((1.0 +. f) /. (1.0 -. f)) /. 2.0)
  | _ => NaN
  };
