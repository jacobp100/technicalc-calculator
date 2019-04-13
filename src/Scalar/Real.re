open PervasivesNoPoly;
/* open PervasivesMath; */
open OptChain;

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

let _floatIsInt = a => Pervasives.(==)(floor(a), a);

type t =
  | Value(Q.t, Constant.t)
  | NaN;

type encoding = [
  | `Value(string, string, Constant.encoding)
  | `NaN
  | `UnknownValue
];

let nan = NaN;
let zero = Value(Q.zero, None);
let one = Value(Q.one, None);
let minusOne = Value(Q.minus_one, None);
let pi = Value(Q.one, Pi);
let e = Value(Q.one, Exp(1));

let isNan = Pervasives.(==)(NaN);

let toFloat = a =>
  switch (a) {
  | Value(aq, ac) => Q.to_float(aq) *. Constant.toFloat(ac)
  | NaN => infinity
  };

let toInt = a =>
  switch (a) {
  | Value(ar, None) when Z.equal(Q.den(ar), Z.one) =>
    switch (Z.to_int(Q.num(ar))) {
    | v => Some(v)
    | exception Z.Overflow => None
    }
  | _ => None
  };

let toQ = a =>
  switch (a) {
  | Value(aq, ac) => Some(aq * Constant.toQ(ac))
  | NaN => None
  };
let toZ = a =>
  switch (a) {
  | Value(aq, None) when Z.equal(Q.den(aq), Z.one) => Some(Q.num(aq))
  | _ => None
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

let ofInt = (~denominator=1, ~constant=Constant.none, numerator) =>
  normalize(Value(Q.of_ints(numerator, denominator), constant));
let ofInts = (~constant=Constant.none, numerator, denominator) =>
  ofInt(numerator, ~denominator, ~constant);

let ofFloat = (~constant=Constant.none, v) =>
  switch (classify_float(v)) {
  | FP_normal
  | FP_subnormal =>
    let magnitude = 1.e6;
    let intMaxF = float_of_int(max_int);
    let numeratorF = v *. magnitude;
    if (abs_float(numeratorF) <% intMaxF && _floatIsInt(numeratorF)) {
      let numerator = int_of_float(numeratorF);
      let denominator = int_of_float(magnitude);
      normalize(ofInts(numerator, denominator, ~constant));
    } else {
      normalize(Value(Q.of_float(v), constant));
    };
  | FP_zero => zero
  | FP_infinite
  | FP_nan => NaN
  };

let ofQ = (~constant=Constant.none, v) => normalize(Value(v, constant));

let of_z = (~constant=Constant.none, ~denominator=?, v) =>
  switch (denominator) {
  | Some(denominator) => ofQ(~constant, Q.make(v, denominator))
  | None => ofQ(~constant, Q.of_bigint(v))
  };

let isInt = a =>
  switch (a) {
  | Value(ar, None) => QUtil.isInt(ar)
  | _ => false
  };

let isNegative = a =>
  switch (a) {
  | Value(ar, _) => Q.lt(ar, Q.zero)
  | _ => false
  };

let equal = (a, b) =>
  switch (a, b) {
  | (Value(aq, ac), Value(bq, bc)) => aq == bq && Constant.equal(ac, bc)
  | _ => false
  };

let ofStringBase = (~constant=Constant.none, base, v) => {
  let (withoutMagnitude, magnitudePart) =
    switch (Util.stringSplitOnChar('e', String.lowercase(v))) {
    | [b, m] => (Some(b), Some(m))
    | [b] => (Some(b), Some("0"))
    | _ => (None, None)
    };
  let (integerPart, decimalPart) =
    switch (withoutMagnitude |? Util.stringSplitOnChar('.')) {
    | Some([i, d]) => (Some(i), Some(d))
    | Some([i]) => (Some(i), Some("0"))
    | _ => (None, None)
    };
  switch (integerPart, decimalPart, magnitudePart) {
  | (Some(integer), Some(decimal), Some(magnitude)) =>
    let num = Z.of_string_base(base, integer ++ decimal);
    let denom = Z.pow(Z.of_int(base), String.length(decimal));
    let exponent = QUtil.expInts(10, int_of_string(magnitude));
    ofQ(Q.make(num, denom) * exponent, ~constant);
  | _ => NaN
  };
};

let ofString = (~constant=Constant.none, v) =>
  ofStringBase(~constant, 10, v);

let toString = (~format=OutputFormat.default, a) => {
  let base = format.base;

  let formatNumber = x =>
    switch (format.mode) {
    | String
    | Tex => x
    | MathML => "<mn>" ++ x ++ "</mn>"
    };
  let formatExponential = ((base, exponent)) =>
    switch (format.mode) {
    | String => base ++ "e" ++ exponent
    | Tex => base ++ "*10^{" ++ exponent ++ "}"
    | MathML =>
      formatNumber(base)
      ++ "<mo>&times;</mo><msup><mn>10</mn>"
      ++ formatNumber(exponent)
      ++ "</msup>"
    };

  switch (format.style, a, toQ(a)) {
  | (Natural, Value(ar, constant), _)
      when
        Z.lt(Q.den(ar), Z.of_int(1000000)) && abs_float(toFloat(a)) <% 1e8 =>
    let (num, den) = (Q.num(ar), Q.den(ar));
    let formatting = NumberFormat.createFormat(~digitSeparators=true, ());
    let minus =
      switch (format.mode, isNegative(a)) {
      | (String | Tex, true) => "-"
      | (MathML, true) => "<mo>-</mo>"
      | (_, false) => ""
      };
    let (top, needsWrap) =
      switch (
        NumberFormat.formatInteger(~base, formatting, Z.abs(num)),
        Constant.toString(~format, constant),
      ) {
      | ("1", "") => (formatNumber("1"), false)
      | ("1", constant) => (constant, false)
      | (numerator, constant) => (formatNumber(numerator) ++ constant, true)
      };

    switch (format.mode, NumberFormat.formatInteger(~base, formatting, den)) {
    | (_, "1") => minus ++ top
    | (String, bottom) => minus ++ top ++ "/" ++ bottom
    | (Tex, bottom) => minus ++ "\\frac{" ++ top ++ "}{" ++ bottom ++ "}"
    | (MathML, denominator) =>
      let top = needsWrap ? "<mrow>" ++ top ++ "</mrow>" : top;
      let bottom = formatNumber(denominator);
      minus ++ "<mfrac>" ++ top ++ bottom ++ "</mfrac>";
    };
  | (Natural | Decimal, _, Some(aq)) =>
    let valueMagnitude = floor(log10(abs_float(Q.to_float(aq))));
    let insideMagnitudeThreshold =
      valueMagnitude
      >=% format.decimalMinMagnitude
      && valueMagnitude
      <=% format.decimalMaxMagnitude;

    if (insideMagnitudeThreshold) {
      NumberFormat.formatDecimal(
        ~base,
        NumberFormat.createFormat(
          ~maxDecimalPlaces=format.precision,
          ~digitSeparators=valueMagnitude >=% 5.,
          (),
        ),
        aq,
      )
      ->formatNumber;
    } else {
      NumberFormat.formatExponential(
        ~base,
        ~exponent=QUtil.magnitude(aq),
        NumberFormat.createFormat(~maxDecimalPlaces=format.precision, ()),
        aq,
      )
      ->formatExponential;
    };
  | (Scientific, _, Some(aq)) =>
    /* Round to multiple of 3 */
    let exponent =
      Pervasives.( * )(Pervasives.(/)(QUtil.magnitude(aq), 3), 3);
    let formatting =
      NumberFormat.createFormat(
        ~minDecimalPlaces=format.precision,
        ~maxDecimalPlaces=format.precision,
        (),
      );
    NumberFormat.formatExponential(~base, ~exponent, formatting, aq)
    ->formatExponential;
  | (_, NaN, _)
  | (_, _, None) =>
    switch (format.mode) {
    | String
    | Tex => "NaN"
    | MathML => "<mi>NaN</mi>"
    }
  };
};

let encode = a =>
  switch (a) {
  | Value(ar, ac) =>
    `Value((
      Q.num(ar)->Z.to_string,
      Q.den(ar)->Z.to_string,
      Constant.encode(ac),
    ))
  | NaN => `NaN
  };
let decode = a =>
  switch (a) {
  | `Value(num, den, const) =>
    Value(
      Q.make(Z.of_string(num), Z.of_string(den)),
      Constant.decode(const),
    )
  | _ => NaN
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
    ofQ(aq + bq, ~constant=ac)
  | (Value(_), Value(_)) =>
    switch (toQ(a), toQ(b)) {
    | (Some(aq), Some(bq)) => ofQ(aq + bq)
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
  | (Value(aq, None), Value(bq, constant)) => ofQ(aq * bq, ~constant)
  | (Value(aq, Sqrt(ac)), Value(bq, Sqrt(bc))) =>
    /* Sqrt is simplifed in ofQ */
    ofQ(aq * bq, ~constant=Sqrt(Z.mul(ac, bc)))
  | (Value(_), Value(_)) =>
    switch (toQ(a), toQ(b)) {
    | (Some(aq), Some(bq)) => ofQ(aq * bq)
    | _ => NaN
    }
  | (NaN, _)
  | (_, NaN) => NaN
  };

let div = (a, b) =>
  switch (a, b) {
  | (_, Value(value, _)) when value == Q.zero => NaN
  | (Value(aq, ac), Value(bq, None)) => ofQ(aq / bq, ~constant=ac)
  | (Value(aq, ac), Value(bq, bc)) when Constant.equal(ac, bc) =>
    ofQ(aq / bq)
  | (Value(aq, Sqrt(ac)), Value(bq, Sqrt(bc)))
      when Z.gt(ac, bc) && Z.equal(Z.rem(ac, bc), Z.zero) =>
    ofQ(aq / bq, ~constant=Sqrt(Z.div(ac, bc)))
  | (Value(_), Value(_)) =>
    switch (toQ(a), toQ(b)) {
    | (Some(aq), Some(bq)) => ofQ(aq / bq)
    | _ => NaN
    }
  | (NaN, _)
  | (_, NaN) => NaN
  };

let sqrt = a =>
  switch (a) {
  | Value(aq, _) when aq < Q.zero => NaN
  | Value(aq, None) when Z.equal(Q.den(aq), Z.one) =>
    ofQ(Q.one, ~constant=Sqrt(Q.num(aq)))
  | Value(_) => ofFloat(sqrt(toFloat(a)))
  | NaN => NaN
  };

let exp = a =>
  switch (a) {
  | Value(aq, None) when Z.equal(Q.den(aq), Z.one) =>
    switch (Z.to_int(Q.num(aq))) {
    | i => ofQ(Q.one, ~constant=Exp(i))
    | exception Z.Overflow => NaN
    }
  | Value(_) => ofFloat(exp(toFloat(a)))
  | NaN => NaN
  };

let rec pow = (a, b) =>
  switch (a, b) {
  | (Value(ar, _), Value(br, _)) when ar == Q.zero && br == Q.zero => NaN
  | (Value(ar, _), Value(_)) when ar == Q.zero => zero
  | (Value(ar, None), Value(_)) when ar == Q.one => one
  | (Value(_), Value(br, None)) when br == Q.of_ints(1, 2) => sqrt(a)
  | (Value(ar, Exp(1)), _) when ar == Q.one => exp(b)
  | (Value(_), Value(br, None)) when br == Q.of_int(2) => mul(a, a)
  | (Value(ar, None), Value(br, None)) when QUtil.isInt(br) && br > Q.zero =>
    switch (Z.to_int(Q.num(br))) {
    | bn => of_z(Z.pow(Q.num(ar), bn), ~denominator=Z.pow(Q.den(ar), bn))
    | exception Z.Overflow => NaN
    }
  | (Value(ar, None), Value(br, None)) when br < Q.zero =>
    pow(ofQ(Q.inv(ar)), ofQ(Q.neg(br)))
  | (Value(_), Value(_)) => ofFloat(toFloat(a) ** toFloat(b))
  | (NaN, _)
  | (_, NaN) => NaN
  };

let log = a =>
  switch (a) {
  | Value(ar, _) when ar <= Q.zero => NaN
  | Value(ar, Exp(ac)) when ar == Q.one => ofInt(ac)
  | Value(_) => ofFloat(log(toFloat(a)))
  | NaN => NaN
  };

let _trigPeriod = a =>
  switch (a) {
  | Value(aq, Pi) => ofQ(QUtil.safeMod(aq, Z.of_int(2)), ~constant=Pi)
  | _ => a
  };

let _toComparable = a =>
  switch (a) {
  | Value(aq, ac) =>
    switch (
      Z.to_int(Q.num(aq)),
      Z.to_int(Q.den(aq)),
      switch (ac) {
      | None => `None
      | Pi => `Pi
      | Sqrt(cc) => `Sqrt(Z.to_int(cc))
      | Exp(cc) => `Exp(cc)
      },
    ) {
    | (n, d, c) => `Value((n, d, c))
    | exception Z.Overflow => `Overflow
    }
  | NaN => `NaN
  };

let sin = a =>
  switch (_trigPeriod(a)->_toComparable) {
  | `Value(0, 1, `None)
  | `Value(1 | 2, 1, `Pi) => ofQ(Q.zero)
  | `Value(1, 2, `Pi) => ofQ(Q.one)
  | `Value(3, 2, `Pi) => ofQ(Q.minus_one)
  | `Value(1 | 2, 3, `Pi) => ofInts(1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(4 | 5, 3, `Pi) => ofInts(-1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(1 | 3, 4, `Pi) => ofInts(1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(5 | 7, 4, `Pi) => ofInts(-1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(1 | 5, 6, `Pi) => ofInts(1, 2)
  | `Value(7 | 11, 6, `Pi) => ofInts(-1, 2)
  | `Value(_)
  | `Overflow => ofFloat(sin(toFloat(a)))
  | `NaN => NaN
  };

let cos = a =>
  switch (_trigPeriod(a)->_toComparable) {
  | `Value(0, 1, `None) => ofQ(Q.one)
  | `Value(2, 1, `Pi) => ofQ(Q.one)
  | `Value(1, 1, `Pi) => ofQ(Q.minus_one)
  | `Value(1 | 3, 2, `Pi) => ofQ(Q.zero)
  | `Value(1 | 5, 3, `Pi) => ofInts(1, 2)
  | `Value(2 | 4, 3, `Pi) => ofInts(-1, 2)
  | `Value(1 | 7, 4, `Pi) => ofInts(1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(3 | 5, 4, `Pi) => ofInts(-1, 2, ~constant=Sqrt(Z.of_int(2)))
  | `Value(1 | 11, 6, `Pi) => ofInts(1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(5 | 7, 6, `Pi) => ofInts(-1, 2, ~constant=Sqrt(Z.of_int(3)))
  | `Value(_)
  | `Overflow => ofFloat(cos(toFloat(a)))
  | `NaN => NaN
  };

let tan = a =>
  switch (_trigPeriod(a)->_toComparable) {
  | `Value(0 | 1 | 2, 1, `Pi) => ofQ(Q.zero)
  | `Value(1 | 5, 4, `Pi) => ofQ(Q.one)
  | `Value(3 | 7, 4, `Pi) => ofQ(Q.minus_one)
  | `Value(1 | 4, 3, `Pi) => ofInt(1, ~constant=Sqrt(Z.of_int(3)))
  | `Value(2 | 5, 3, `Pi) => ofInt(-1, ~constant=Sqrt(Z.of_int(3)))
  | `Value(1 | 7, 6, `Pi) => ofInts(1, 3, ~constant=Sqrt(Z.of_int(3)))
  | `Value(5 | 11, 6, `Pi) => ofInts(-1, 3, ~constant=Sqrt(Z.of_int(3)))
  | `Value(1 | 3, 2, `Pi) => NaN
  | `Value(_)
  | `Overflow => ofFloat(tan(toFloat(a)))
  | `NaN => NaN
  };

let asin = a =>
  switch (_toComparable(a)) {
  | `Value((-1), 1, `None) => ofInts(-1, 2, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(3)) => ofInts(-1, 3, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(2)) => ofInts(-1, 4, ~constant=Pi)
  | `Value((-1), 2, `None) => ofInts(-1, 6, ~constant=Pi)
  | `Value(0, 1, `None) => ofQ(Q.zero)
  | `Value(1, 2, `None) => ofInts(1, 6, ~constant=Pi)
  | `Value(1, 2, `Sqrt(2)) => ofInts(1, 4, ~constant=Pi)
  | `Value(1, 2, `Sqrt(3)) => ofInts(1, 3, ~constant=Pi)
  | `Value(1, 1, `None) => ofInts(1, 2, ~constant=Pi)
  | `Value(_)
  | `Overflow => ofFloat(asin(toFloat(a)))
  | `NaN => NaN
  };

let acos = a =>
  switch (_toComparable(a)) {
  | `Value((-1), 1, `None) => ofInt(1, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(3)) => ofInts(5, 6, ~constant=Pi)
  | `Value((-1), 2, `Sqrt(2)) => ofInts(3, 4, ~constant=Pi)
  | `Value((-1), 2, `None) => ofInts(2, 3, ~constant=Pi)
  | `Value(0, 1, `None) => ofInts(1, 2, ~constant=Pi)
  | `Value(1, 2, `None) => ofInts(1, 3, ~constant=Pi)
  | `Value(1, 2, `Sqrt(2)) => ofInts(1, 4, ~constant=Pi)
  | `Value(1, 2, `Sqrt(3)) => ofInts(1, 6, ~constant=Pi)
  | `Value(1, 1, `None) => ofQ(Q.zero)
  | `Value(_)
  | `Overflow => ofFloat(acos(toFloat(a)))
  | `NaN => NaN
  };

let atan = a =>
  switch (_toComparable(a)) {
  | `Value((-1), 1, `Sqrt(3)) => ofInts(-1, 3, ~constant=Pi)
  | `Value((-1), 1, `None) => ofInts(-1, 4, ~constant=Pi)
  | `Value((-1), 3, `Sqrt(3)) => ofInts(-1, 6, ~constant=Pi)
  | `Value(0, 1, `None) => ofQ(Q.zero)
  | `Value(1, 3, `Sqrt(3)) => ofInts(1, 6, ~constant=Pi)
  | `Value(1, 1, `None) => ofInts(1, 4, ~constant=Pi)
  | `Value(1, 1, `Sqrt(3)) => ofInts(1, 3, ~constant=Pi)
  | `Value(_)
  | `Overflow => ofFloat(atan(toFloat(a)))
  | `NaN => NaN
  };

let factorial = n =>
  switch (toFloat(n)) {
  | f when FloatUtil.isInt(f) && f >=% 0. && f <=% 1000. =>
    let i = int_of_float(f);
    let fact = ref(Z.one);
    for (mul in 2 to i) {
      fact := Z.mul(fact^, Z.of_int(mul));
    };
    ofQ(Q.of_bigint(fact^));
  | f when f >=% 0. =>
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let (p, g) = (FactorialUtil.p, FactorialUtil.g);
    let x = ref(p[0]);
    for (i in 1 to Pervasives.(-)(Belt.Array.length(p), 1)) {
      x := x^ +. p[i] /. (f +. float_of_int(i));
    };
    let t = f +. g +. 0.5;
    let value =
      Pervasives.sqrt(2.0 *. FloatUtil.pi)
      *. t
      ** (f +. 0.5)
      *. Pervasives.exp(-. t)
      *. x^;
    ofFloat(value);
  | _ => NaN
  };

let _mapFloat = (fn, x) =>
  switch (x) {
  | Value(_) => ofFloat(fn(toFloat(x)))
  | NaN => NaN
  };

let _bounds = (~lower=?, ~upper=?, x) =>
  switch (x) {
  | Value(_) => FloatUtil.bounds(~lower?, ~upper?, toFloat(x))
  | NaN => `Outside
  };

let sinh = _mapFloat(sinh);
let asinh = _mapFloat(FloatUtil.asinh);
let cosh = _mapFloat(cosh);
let acosh = x =>
  switch (_bounds(~lower=1.0, x)) {
  | `Inside(f) => ofFloat(FloatUtil.acosh(f))
  | `LowerBound => zero
  | _ => NaN
  };
let tanh = _mapFloat(tanh);
let atanh = x =>
  switch (_bounds(~lower=-1.0, ~upper=1.0, x)) {
  | `Inside(f) => ofFloat(FloatUtil.atanh(f))
  | _ => NaN
  };
