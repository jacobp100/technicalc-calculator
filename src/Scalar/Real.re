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

type fractionFormat = {
  minus_sign: bool,
  numerator: option(Zt.t),
  constant: option(Constant.t),
  denominator: option(Zt.t),
};

type formatting =
  | Fraction(fractionFormat)
  | Decimal(float)
  | Scientific(float)
  | MathError;

let format_fraction = (format: Formatting.format, a) =>
  switch (format, a) {
  | (Natural, Value(ar, ac)) when Zt.lt(Qt.den(ar), Zt.of_int(1000000)) =>
    let num = Qt.num(ar);
    let den = Qt.den(ar);

    let showConstant = !Constant.equal(ac, None);
    let showNumerator = !Zt.equal(num, Zt.one) || !showConstant;
    let showDenominator = !Zt.equal(den, Zt.one);

    let minus_sign = !showNumerator && Qt.lt(ar, Qt.zero);
    let numerator = showNumerator ? Some(num) : None;
    let constant = showConstant ? Some(ac) : None;
    let denominator = showDenominator ? Some(den) : None;

    Fraction({minus_sign, numerator, constant, denominator});
  | (Natural | Numerical, Value(_)) => Decimal(to_float(a))
  | (Scientific, Value(_)) => Scientific(to_float(a))
  | (_, NaN) => MathError
  };

let to_string = a =>
  switch (format_fraction(Natural, a)) {
  | Fraction({minus_sign, numerator, constant, denominator}) =>
    let body = ref("");
    if (minus_sign) {
      body := body^ ++ "-";
    };
    switch (numerator) {
    | Some(n) => body := body^ ++ Zt.to_string(n)
    | _ => ()
    };
    switch (constant) {
    | Some(c) => body := body^ ++ Constant.to_string(c)
    | _ => ()
    };
    switch (denominator) {
    | Some(d) => body := body^ ++ "/" ++ Zt.to_string(d)
    | _ => ()
    };
    body^;
  | Decimal(f)
  | Scientific(f) => string_of_float(f)
  | MathError => "NaN"
  };

let to_latex = a =>
  switch (format_fraction(Natural, a)) {
  | Fraction({minus_sign, numerator, constant, denominator}) =>
    let body = ref("");
    switch (numerator) {
    | Some(n) => body := body^ ++ Zt.to_string(n)
    | _ => ()
    };
    switch (constant) {
    | Some(c) => body := body^ ++ Constant.to_latex(c)
    | _ => ()
    };
    switch (denominator) {
    | Some(d) => body := "\\frac{" ++ body^ ++ "}{" ++ Zt.to_string(d) ++ "}"
    | _ => ()
    };
    if (minus_sign) {
      body := body^ ++ "-";
    };
    body^;
  | Decimal(f) => string_of_float(f)
  | Scientific(f) => string_of_float(f)
  | MathError => "NaN"
  };

let q_is_integer = a => Zt.equal(Qt.den(a), Zt.one);
let is_integer = a =>
  switch (a) {
  | Value(ar, None) => q_is_integer(ar)
  | _ => false
  };

let equal = (a, b) =>
  switch (a, b) {
  | (Value(aq, ac), Value(bq, bc)) =>
    Qt.equal(aq, bq) && Constant.equal(ac, bc)
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
      when q_is_integer(ar) && q_is_integer(br) && br > Qt.zero =>
    let (an, bn) = (Qt.num(ar), Qt.num(br));
    of_z(Zt.pow(an, bn));
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

type boundary =
  | BothBound
  | UpperBound
  | LowerBound
  | Inside(float)
  | Outside;

let _check_bounds = (~lower=?, ~upper=?, x) =>
  switch (x) {
  | Value(_) =>
    let f = to_float(x);
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
  | _ => NaN
  };
let arctan = _map_float(atan);
let tanh = _map_float(tanh);
let arctanh = x =>
  switch (_check_bounds(~lower=-1.0, ~upper=1.0, x)) {
  | Inside(f) => of_float(Pervasives.log((1.0 +. f) /. (1.0 -. f)) /. 2.0)
  | _ => NaN
  };
