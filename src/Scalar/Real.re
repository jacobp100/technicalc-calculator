module Zt = Z.Bigint;
module Qt = Q.Bigint;

let (==) = Qt.equal;
let (+) = Qt.add;
/* let (-) = Qt.sub; */
let ( * ) = Qt.mul;
let (/) = Qt.div;
/* let (mod) = Qt.rem; */
let (~-) = Qt.neg;

let _float_is_int = a => Pervasives.(==)(floor(a), a);

type t =
  | Value(Qt.t, Constant.t)
  | NaN;

let to_float = a =>
  switch (a) {
  | Value(aq, ac) => Qt.to_float(aq) *. Constant.to_float(ac)
  | NaN => infinity
  };

let normalize = a =>
  switch (a) {
  | Value(aq, ac) when aq == Qt.zero && !Constant.equal(ac, None) =>
    Value(Qt.zero, None)
  | Value(_, Sqrt(ac)) when Zt.equal(ac, Zt.zero) => Value(Qt.zero, None)
  | _ => a
  };

let of_int = (~denominator=1, ~constant=Constant.none, numerator) =>
  normalize(Value(Qt.of_ints(numerator, denominator), constant));

let of_float = (~constant=Constant.none, v) => {
  let magnitude = 1.e6;
  let int_max_f = float_of_int(max_int);
  let numerator_f = v *. magnitude;
  if (abs_float(numerator_f) < int_max_f && _float_is_int(numerator_f)) {
    let numerator = int_of_float(numerator_f);
    let denominator = int_of_float(magnitude);
    normalize(of_int(numerator, ~denominator, ~constant));
  } else {
    normalize(Value(Qt.of_float(v), constant));
  };
};

let of_q = (~constant=Constant.none, v) => normalize(Value(v, constant));

let of_z = (~constant=Constant.none, v) =>
  of_q(~constant, Qt.make(v, Zt.one));

let nan = NaN;
let zero = Value(Qt.zero, None);
let one = Value(Qt.one, None);
let minus_one = Value(Qt.minus_one, None);
let pi = Value(Qt.one, Pi);
let e = Value(Qt.one, Exp(Zt.one));

let is_nan = Pervasives.(==)(NaN);

let q_is_integer = a => Zt.equal(Qt.den(a), Zt.one);

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
  | (Value(aq, Sqrt(ac)), Value(bq, Sqrt(bc))) when Zt.equal(ac, bc) =>
    of_q(aq * bq * Qt.make(ac, Zt.one))
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
      when ac > bc && Zt.equal(Zt.rem(ac, bc), Zt.zero) =>
    of_q(aq / bq, ~constant=Sqrt(Zt.div(ac, bc)))
  | (Value(_, _), Value(_, _)) => of_float(to_float(a) /. to_float(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let sqrt = a =>
  switch (a) {
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
  | PiFraction(int, int)
  | Float
  | NaN;

let _trig_period = a =>
  switch (a) {
  | Value(aq, _) when aq == Qt.zero => PiFraction(0, 1)
  | Value(aq, Pi) =>
    let denominator = Qt.den(aq);
    let divisor = Zt.mul(Zt.of_int(2), denominator);
    let numerator = Zt.rem(Qt.num(aq), divisor);
    /* Handle negative numerators */
    let numerator = Zt.rem(Zt.add(numerator, divisor), divisor);
    switch (Zt.to_int(numerator), Zt.to_int(denominator)) {
    | (n, d) => PiFraction(n, d)
    | exception Zt.Overflow => Float
    };
  | Value(_, _) => Float
  | NaN => NaN
  };

let sin = a =>
  switch (_trig_period(a)) {
  | PiFraction(0 | 1 | 2, 1) => of_q(Qt.zero)
  | PiFraction(1 | 5, 2) => of_q(Qt.one)
  | PiFraction(3 | 7, 2) => of_q(Qt.minus_one)
  | PiFraction(1 | 2, 3) =>
    of_int(1, ~denominator=2, ~constant=Sqrt(Zt.of_int(3)))
  | PiFraction(4 | 5, 3) =>
    of_int(-1, ~denominator=2, ~constant=Sqrt(Zt.of_int(3)))
  | PiFraction(1 | 3, 4) =>
    of_int(1, ~denominator=2, ~constant=Sqrt(Zt.of_int(2)))
  | PiFraction(5 | 7, 4) =>
    of_int(-1, ~denominator=2, ~constant=Sqrt(Zt.of_int(2)))
  | PiFraction(1 | 5, 6) => of_int(1, ~denominator=2)
  | PiFraction(7 | 11, 6) => of_int(-1, ~denominator=2)
  | PiFraction(_, _)
  | Float => of_float(sin(to_float(a)))
  | NaN => NaN
  };

let cos = a => sin(add(a, of_int(1, ~denominator=2, ~constant=Pi)));

let tan = a =>
  switch (_trig_period(a)) {
  | PiFraction(0 | 1 | 2, 1) => of_q(Qt.zero)
  | PiFraction(1 | 5, 4) => of_q(Qt.one)
  | PiFraction(3 | 7, 4) => of_q(Qt.minus_one)
  | PiFraction(1 | 4, 3) => of_q(Qt.one, ~constant=Sqrt(Zt.of_int(3)))
  | PiFraction(2 | 5, 3) =>
    of_q(Qt.minus_one, ~constant=Sqrt(Zt.of_int(3)))
  | PiFraction(1 | 7, 6) =>
    of_int(1, ~denominator=3, ~constant=Sqrt(Zt.of_int(3)))
  | PiFraction(5 | 11, 6) =>
    of_int(-1, ~denominator=3, ~constant=Sqrt(Zt.of_int(3)))
  | PiFraction(1 | 3, 2) => NaN
  | PiFraction(_, _)
  | Float => of_float(tan(to_float(a)))
  | NaN => NaN
  };

let pow = (a, b) =>
  switch (a, b) {
  | (Value(_), Value(ar, None)) when ar == Qt.of_ints(1, 2) => sqrt(a)
  | (Value(ar, Exp(ac)), _) when ar == Qt.one && Zt.equal(ac, Zt.one) =>
    exp(b)
  | (Value(ar, None), Value(br, None))
      when q_is_integer(ar) && q_is_integer(br) =>
    let (an, bn) = (Qt.num(ar), Qt.num(br));
    of_z(Zt.pow(an, bn));
  | (Value(ar, Sqrt(ac)), Value(br, None))
      when ar == Qt.one && br == Qt.of_int(2) =>
    of_z(ac)
  | (Value(_), Value(_)) => of_float(to_float(a) ** to_float(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let log = a =>
  switch (a) {
  | Value(ar, Exp(ac)) when ar == Qt.one => of_z(ac)
  | Value(_) => of_float(log(to_float(a)))
  | NaN => NaN
  };

let to_string = a =>
  switch (a) {
  | Value(ar, ac) when Zt.lt(Qt.den(ar), Zt.of_int(1000000)) =>
    let num = Qt.num(ar);
    let den = Qt.den(ar);

    let constant_str = Constant.to_string(ac);
    let numerator_str =
      Zt.equal(num, Zt.one) && Pervasives.(!=)(constant_str, "") ?
        "" : Zt.to_string(num);
    let denominator_str =
      Zt.equal(den, Zt.one) ? "" : "/" ++ Zt.to_string(den);

    numerator_str ++ constant_str ++ denominator_str;
  | Value(_) => string_of_float(to_float(a))
  | NaN => "NaN"
  };
