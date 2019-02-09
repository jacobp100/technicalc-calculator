open PervasivesNoPoly;
open PervasivesMath;

let (==) = Q.equal;
let (+) = Q.add;
/* let (-) = Q.sub; */
let ( * ) = Q.mul;
let (/) = Q.div;
/* let (mod) = Q.rem; */
let (~-) = Q.neg;
let (<) = Q.lt;
let (<=) = Q.leq;
let (>) = Q.gt;
/* let (>=) = Q.geq; */

let (|?) = (x, f) =>
  switch (x) {
  | Some(a) => Some(f(a))
  | None => None
  };

let _float_is_int = a => Pervasives.(==)(floor(a), a);

type t =
  | Value(Q.t, Constant.t)
  | NaN;

let nan = NaN;
let zero = Value(Q.zero, None);
let one = Value(Q.one, None);
let minus_one = Value(Q.minus_one, None);
let pi = Value(Q.one, Pi);
let e = Value(Q.one, Exp(Z.one));

let is_nan = Pervasives.(==)(NaN);

let to_float = a =>
  switch (a) {
  | Value(aq, ac) => Q.to_float(aq) *. Constant.to_float(ac)
  | NaN => infinity
  };

let to_int = a =>
  switch (a) {
  | Value(ar, None) when Z.equal(Q.den(ar), Z.one) =>
    switch (Z.to_int(Q.num(ar))) {
    | v => Some(v)
    | exception Z.Overflow => None
    }
  | _ => None
  };

let to_q = a =>
  switch (a) {
  | Value(aq, ac) =>
    let f = Constant.to_float(ac);
    switch (classify_float(f)) {
    | FP_normal
    | FP_subnormal => Some(aq * Q.of_float(f))
    | FP_zero => Some(Q.zero)
    | FP_infinite
    | FP_nan => None
    };
  | NaN => None
  };

let normalize = a =>
  /* Note redundant branches are for optimisation */
  switch (a) {
  | Value(_, None) => a
  | Value(aq, _) when aq == Q.zero => Value(Q.zero, None)
  | Value(_, Pi) => a
  | Value(aq, ac) =>
    let (multiplier, constant) = Constant.simplify(ac);
    Value(aq * Q.of_bigint(multiplier), constant);
  | _ => a
  };

let of_int = (~denominator=1, ~constant=Constant.none, numerator) =>
  normalize(Value(Q.of_ints(numerator, denominator), constant));
let of_ints = (~constant=Constant.none, numerator, denominator) =>
  of_int(numerator, ~denominator, ~constant);

let of_float = (~constant=Constant.none, v) =>
  switch (classify_float(v)) {
  | FP_normal
  | FP_subnormal =>
    let magnitude = 1.e6;
    let int_max_f = float_of_int(max_int);
    let numerator_f = v *. magnitude;
    if (abs_float(numerator_f) <% int_max_f && _float_is_int(numerator_f)) {
      let numerator = int_of_float(numerator_f);
      let denominator = int_of_float(magnitude);
      normalize(of_ints(numerator, denominator, ~constant));
    } else {
      normalize(Value(Q.of_float(v), constant));
    };
  | FP_zero => zero
  | FP_infinite
  | FP_nan => NaN
  };

let of_q = (~constant=Constant.none, v) => normalize(Value(v, constant));

let of_z = (~constant=Constant.none, ~denominator=?, v) =>
  switch (denominator) {
  | Some(denominator) => of_q(~constant, Q.make(v, denominator))
  | None => of_q(~constant, Q.of_bigint(v))
  };

let of_string = (~constant=Constant.none, v) => {
  let (withoutMagnitude, magnitudePart) =
    switch (Util.string_split_on_char('e', String.lowercase(v))) {
    | [b, baseM] =>
      let m =
        baseM.[0] ==% '+' ?
          String.sub(baseM, 1, String.length(baseM) -% 1) : baseM;
      (Some(b), Some(m));
    | [b] => (Some(b), Some("0"))
    | _ => (None, None)
    };
  let (integerPart, decimalPart) =
    switch (withoutMagnitude |? Util.string_split_on_char('.')) {
    | Some([i, d]) => (Some(i), Some(d))
    | Some([i]) => (Some(i), Some("0"))
    | _ => (None, None)
    };
  switch (integerPart, decimalPart, magnitudePart) {
  | (Some(integer), Some(decimal), Some(magnitude)) =>
    let num = Z.of_string(integer ++ decimal);
    let denom = Z.pow(Z.of_int(10), Z.of_int(String.length(decimal)));
    let exponent = Util.q_exp_10(Z.of_string(magnitude));
    of_q(Q.make(num, denom) * exponent, ~constant);
  | _ => NaN
  };
};

let to_string = (~format=OutputFormat.default, a) => {
  let exponent_format =
    switch (format.mode) {
    | Latex => Some("\\times10^{$}")
    | _ => None
    };

  switch (format.style, a, to_q(a)) {
  | (Natural, Value(ar, constant), _)
      when
        Z.lt(Q.den(ar), Z.of_int(1000000))
        && abs_float(to_float(a))
        <% 1e8 =>
    let (num, den) = (Q.num(ar), Q.den(ar));
    let formatting = NumberFormat.create_format(~digit_separators=true, ());
    let minus = Z.lt(num, Z.zero) ? "-" : "";
    let top =
      switch (
        NumberFormat.format_integer(formatting, Z.abs(num)),
        Constant.to_string(~format, constant),
      ) {
      | ("1", "") => "1"
      | ("1", constant) => constant
      | (numerator, constant) => numerator ++ constant
      };

    switch (format.mode, NumberFormat.format_integer(formatting, den)) {
    | (_, "1") => minus ++ top
    | (String, denominator) => minus ++ top ++ "/" ++ denominator
    | (Latex, denominator) =>
      minus ++ "\\frac{" ++ top ++ "}{" ++ denominator ++ "}"
    };
  | (Natural | Decimal, _, Some(aq)) =>
    let value_magnitude = floor(log10(abs_float(Q.to_float(aq))));
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
        aq,
      );
    } else {
      NumberFormat.format_exponential(
        ~exponent=Util.q_magnitude(aq),
        ~exponent_format?,
        NumberFormat.create_format(~max_decimal_places=format.precision, ()),
        aq,
      );
    };
  | (Scientific, _, Some(aq)) =>
    let exponent =
      Z.mul(Z.div(Util.q_magnitude(aq), Z.of_int(3)), Z.of_int(3));
    NumberFormat.format_exponential(
      ~exponent,
      ~exponent_format?,
      NumberFormat.create_format(
        ~min_decimal_places=format.precision,
        ~max_decimal_places=format.precision,
        (),
      ),
      aq,
    );
  | (_, NaN, _)
  | (_, _, None) => "NaN"
  };
};

let q_is_integer = a => Z.equal(Q.den(a), Z.one);
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

let abs = a =>
  switch (a) {
  | Value(ar, ac) => Value(Q.abs(ar), ac)
  | _ => NaN
  };

let add = (a, b) =>
  switch (a, b) {
  | (Value(aq, _), Value(_)) when aq == Q.zero => b
  | (Value(_), Value(bq, _)) when bq == Q.zero => a
  | (Value(aq, ac), Value(bq, bc)) when Constant.equal(ac, bc) =>
    of_q(aq + bq, ~constant=ac)
  | (Value(_), Value(_)) =>
    switch (to_q(a), to_q(b)) {
    | (Some(aq), Some(bq)) => of_q(aq + bq)
    | _ => NaN
    }
  | (NaN, _)
  | (_, NaN) => NaN
  };

let sub = (a, b) => add(a, neg(b));

let mul = (a, b) =>
  switch (a, b) {
  | (Value(aq, _), Value(_)) when aq == Q.zero => zero
  | (Value(_), Value(bq, _)) when bq == Q.zero => zero
  | (Value(aq, constant), Value(bq, None))
  | (Value(aq, None), Value(bq, constant)) => of_q(aq * bq, ~constant)
  | (Value(aq, Sqrt(ac)), Value(bq, Sqrt(bc))) =>
    /* Sqrt is simplifed in of_q */
    of_q(aq * bq, ~constant=Sqrt(Z.mul(ac, bc)))
  | (Value(_), Value(_)) =>
    switch (to_q(a), to_q(b)) {
    | (Some(aq), Some(bq)) => of_q(aq * bq)
    | _ => NaN
    }
  | (NaN, _)
  | (_, NaN) => NaN
  };

let div = (a, b) =>
  switch (a, b) {
  | (_, Value(value, _)) when value == Q.zero => NaN
  | (Value(aq, ac), Value(bq, None)) => of_q(aq / bq, ~constant=ac)
  | (Value(aq, ac), Value(bq, bc)) when Constant.equal(ac, bc) =>
    of_q(aq / bq)
  | (Value(aq, Sqrt(ac)), Value(bq, Sqrt(bc)))
      when Z.gt(ac, bc) && Z.equal(Z.rem(ac, bc), Z.zero) =>
    of_q(aq / bq, ~constant=Sqrt(Z.div(ac, bc)))
  | (Value(_), Value(_)) =>
    switch (to_q(a), to_q(b)) {
    | (Some(aq), Some(bq)) => of_q(aq / bq)
    | _ => NaN
    }
  | (NaN, _)
  | (_, NaN) => NaN
  };

let sqrt = a =>
  switch (a) {
  | Value(aq, _) when aq < Q.zero => NaN
  | Value(aq, None) when Z.equal(Q.den(aq), Z.one) =>
    of_q(Q.one, ~constant=Sqrt(Q.num(aq)))
  | Value(_) => of_float(sqrt(to_float(a)))
  | NaN => NaN
  };

let exp = a =>
  switch (a) {
  | Value(aq, None) when Z.equal(Q.den(aq), Z.one) =>
    of_q(Q.one, ~constant=Exp(Q.num(aq)))
  | Value(_) => of_float(exp(to_float(a)))
  | NaN => NaN
  };

let pow = (a, b) =>
  switch (a, b) {
  | (Value(ar, _), Value(br, _)) when ar == Q.zero && br == Q.zero => NaN
  | (Value(ar, _), Value(_)) when ar == Q.zero => zero
  | (Value(ar, None), Value(_)) when ar == Q.one => one
  | (Value(_), Value(br, None)) when br == Q.of_ints(1, 2) => sqrt(a)
  | (Value(ar, Exp(ac)), _) when ar == Q.one && Z.equal(ac, Z.one) =>
    exp(b)
  | (Value(_), Value(br, None)) when br == Q.of_int(2) => mul(a, a)
  | (Value(ar, None), Value(br, None)) when q_is_integer(br) && br > Q.zero =>
    let (an, ad, bn) = (Q.num(ar), Q.den(ar), Q.num(br));
    of_q(Q.make(Z.pow(an, bn), Z.pow(ad, bn)));
  | (Value(_), Value(_)) => of_float(to_float(a) ** to_float(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let log = a =>
  switch (a) {
  | Value(ar, _) when ar <= Q.zero => NaN
  | Value(ar, Exp(ac)) when ar == Q.one => of_z(ac)
  | Value(_) => of_float(log(to_float(a)))
  | NaN => NaN
  };

let _trig_period = a =>
  switch (a) {
  | Value(aq, Pi) => of_q(Util.q_safe_mod_z(aq, Z.of_int(2)), ~constant=Pi)
  | _ => a
  };

let _to_comparable = a =>
  switch (a) {
  | Value(aq, ac) =>
    switch (
      Z.to_int(Q.num(aq)),
      Z.to_int(Q.den(aq)),
      switch (ac) {
      | None => `None
      | Pi => `Pi
      | Sqrt(cc) => `Sqrt(Z.to_int(cc))
      | Exp(cc) => `Exp(Z.to_int(cc))
      },
    ) {
    | (n, d, c) => `Value((n, d, c))
    | exception Z.Overflow => `Overflow
    }
  | NaN => `NaN
  };

let sin = a =>
  switch (a |> _trig_period |> _to_comparable) {
  | `Value(0, 1, `None)
  | `Value(1 | 2, 1, `Pi) => of_q(Q.zero)
  | `Value(1, 2, `Pi) => of_q(Q.one)
  | `Value(3, 2, `Pi) => of_q(Q.minus_one)
  | `Value(1 | 2, 3, `Pi) => of_ints(1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(4 | 5, 3, `Pi) => of_ints(-1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(1 | 3, 4, `Pi) => of_ints(1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(5 | 7, 4, `Pi) => of_ints(-1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(1 | 5, 6, `Pi) => of_ints(1, 2)
  | `Value(7 | 11, 6, `Pi) => of_ints(-1, 2)
  | `Value(_)
  | `Overflow => of_float(sin(to_float(a)))
  | `NaN => NaN
  };

let cos = a =>
  switch (a |> _trig_period |> _to_comparable) {
  | `Value(0, 1, `None) => of_q(Q.one)
  | `Value(2, 1, `Pi) => of_q(Q.one)
  | `Value(1, 1, `Pi) => of_q(Q.minus_one)
  | `Value(1 | 3, 2, `Pi) => of_q(Q.zero)
  | `Value(1 | 5, 3, `Pi) => of_ints(1, 2)
  | `Value(2 | 4, 3, `Pi) => of_ints(-1, 2)
  | `Value(1 | 7, 4, `Pi) => of_ints(1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(3 | 5, 4, `Pi) => of_ints(-1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(1 | 11, 6, `Pi) => of_ints(1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(5 | 7, 6, `Pi) => of_ints(-1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(_)
  | `Overflow => of_float(cos(to_float(a)))
  | `NaN => NaN
  };

let tan = a =>
  switch (a |> _trig_period |> _to_comparable) {
  | `Value(0 | 1 | 2, 1, `Pi) => of_q(Q.zero)
  | `Value(1 | 5, 4, `Pi) => of_q(Q.one)
  | `Value(3 | 7, 4, `Pi) => of_q(Q.minus_one)
  | `Value(1 | 4, 3, `Pi) => of_int(1, ~constant=Sqrt(Z.of_int(3)))
  | `Value(2 | 5, 3, `Pi) => of_int(-1, ~constant=Sqrt(Z.of_int(3)))
  | `Value(1 | 7, 6, `Pi) => of_ints(1, 3, ~constant=Sqrt(Z.of_int(3)))
  | `Value(5 | 11, 6, `Pi) => of_ints(-1, 3, ~constant=Sqrt(Z.of_int(3)))
  | `Value(1 | 3, 2, `Pi) => NaN
  | `Value(_)
  | `Overflow => of_float(tan(to_float(a)))
  | `NaN => NaN
  };

let asin = a =>
  switch (_to_comparable(a)) {
  | `Value((-1), 1, `None) => of_ints(-1, 2, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(3)) => of_ints(-1, 3, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(2)) => of_ints(-1, 4, ~constant=Pi)
  | `Value((-1), 2, `None) => of_ints(-1, 6, ~constant=Pi)
  | `Value(0, 1, `None) => of_q(Q.zero)
  | `Value(1, 2, `None) => of_ints(1, 6, ~constant=Pi)
  | `Value(1, 2, `Sqrt(2)) => of_ints(1, 4, ~constant=Pi)
  | `Value(1, 2, `Sqrt(3)) => of_ints(1, 3, ~constant=Pi)
  | `Value(1, 1, `None) => of_ints(1, 2, ~constant=Pi)
  | `Value(_)
  | `Overflow => of_float(asin(to_float(a)))
  | `NaN => NaN
  };

let acos = a =>
  switch (_to_comparable(a)) {
  | `Value((-1), 1, `None) => of_int(1, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(3)) => of_ints(5, 6, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(2)) => of_ints(3, 4, ~constant=Pi)
  | `Value((-1), 2, `None) => of_ints(2, 3, ~constant=Pi)
  | `Value(0, 1, `None) => of_ints(1, 2, ~constant=Pi)
  | `Value(1, 2, `None) => of_ints(1, 3, ~constant=Pi)
  | `Value(1, 2, `Sqrt(2)) => of_ints(1, 4, ~constant=Pi)
  | `Value(1, 2, `Sqrt(3)) => of_ints(1, 6, ~constant=Pi)
  | `Value(1, 1, `None) => of_q(Q.zero)
  | `Value(_)
  | `Overflow => of_float(acos(to_float(a)))
  | `NaN => NaN
  };

let atan = a =>
  switch (_to_comparable(a)) {
  | `Value((-1), 1, `Sqrt(3)) => of_ints(-1, 3, ~constant=Pi)
  | `Value((-1), 1, `None) => of_ints(-1, 4, ~constant=Pi)
  | `Value((-1), 3, `Sqrt(3)) => of_ints(-1, 6, ~constant=Pi)
  | `Value(0, 1, `None) => of_q(Q.zero)
  | `Value(1, 3, `Sqrt(3)) => of_ints(1, 6, ~constant=Pi)
  | `Value(1, 1, `None) => of_ints(1, 4, ~constant=Pi)
  | `Value(1, 1, `Sqrt(3)) => of_ints(1, 3, ~constant=Pi)
  | `Value(_)
  | `Overflow => of_float(atan(to_float(a)))
  | `NaN => NaN
  };

let factorial = n =>
  switch (to_float(n)) {
  | f when Util.f_is_int(f) && f >=% 0. && f <=% 1000. =>
    let i = int_of_float(f);
    let fact = ref(Z.one);
    for (mul in 2 to i) {
      fact := Z.mul(fact^, Z.of_int(mul));
    };
    of_q(Q.of_bigint(fact^));
  | f when f >=% 0. =>
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let (p, g) = (FactorialUtil.p, FactorialUtil.g);
    let x = ref(p[0]);
    for (i in 1 to (Pervasives.(-)(Array.length(p), 1))) {
      x := x^ +. p[i] /. (f +. float_of_int(i));
    };
    let t = f +. g +. 0.5;
    let value =
      Pervasives.sqrt(2.0 *. Util.pi)
      *. t
      ** (f +. 0.5)
      *. Pervasives.exp(-. t)
      *. x^;
    of_float(value);
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
  | NaN => `Outside
  };

let sinh = _map_float(sinh);
let asinh = _map_float(Util.asinh);
let cosh = _map_float(cosh);
let acosh = x =>
  switch (_check_bounds(~lower=1.0, x)) {
  | `Inside(f) => of_float(Util.acosh(f))
  | `LowerBound => zero
  | _ => NaN
  };
let tanh = _map_float(tanh);
let atanh = x =>
  switch (_check_bounds(~lower=-1.0, ~upper=1.0, x)) {
  | `Inside(f) => of_float(Util.atanh(f))
  | _ => NaN
  };
