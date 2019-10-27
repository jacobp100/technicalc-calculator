type scalar = [
  | `Zero
  | `Real(Real.t)
  | `Imag(Real.t)
  | `Complex(Real.t, Real.t)
];
type percent = [ | `Percent(scalar)];
type matrix = [ | `Matrix(Matrix.t(scalar))];
type vector = [ | `Vector(array(scalar))];
type value = [ percent | scalar | matrix | vector | `NaN];

external valueOfPercent: percent => value = "%identity";
external valueOfScalar: scalar => value = "%identity";
external valueOfMatrix: matrix => value = "%identity";
external valueOfVector: vector => value = "%identity";

let zero: value = `Zero;
let one: value = `Real(Rational(1, 1, Unit));
let minusOne: value = `Real(Rational(-1, 1, Unit));
let i: value = `Imag(Rational(1, 1, Unit));
let minusI: value = `Imag(Rational(-1, 1, Unit));
let pi: value = `Real(Rational(1, 1, Pi));
let e: value = `Real(Rational(1, 1, Exp(1)));
let nan = `NaN;

let scalarIsNaN = (a: scalar) =>
  switch (a) {
  | `Zero => false
  | `Real(v)
  | `Imag(v) => Real.isNaN(v)
  | `Complex(re, im) => Real.isNaN(re) || Real.isNaN(im)
  };

let normalizeScalar = (v: scalar): scalar =>
  switch (v) {
  | `Zero
  | `Real(Rational(0, _, _))
  | `Imag(Rational(0, _, _))
  | `Complex(Rational(0, _, _), Rational(0, _, _)) => `Zero
  | `Complex(Rational(0, _, _), v) => `Imag(v)
  | `Complex(v, Rational(0, _, _)) => `Real(v)
  | `Real(_)
  | `Imag(_)
  | `Complex(_) => v
  };

let normalize = (v: value): value =>
  switch (v) {
  | `Zero => `Zero
  | (`Real(v) | `Imag(v)) as scalar =>
    Real.isNaN(v) ? `NaN : normalizeScalar(scalar)->valueOfScalar
  | `Complex(re, im) as scalar =>
    Real.isNaN(re) || Real.isNaN(im)
      ? `NaN : normalizeScalar(scalar)->valueOfScalar
  | `Percent(p) =>
    if (scalarIsNaN(p)) {
      `NaN;
    } else {
      let normalized = normalizeScalar(p);
      normalized === p ? v : `Percent(normalizeScalar(p));
    }
  | `Vector(elements) =>
    let isNaN = elements->Belt.Array.some(scalarIsNaN);
    isNaN ? `NaN : `Vector(elements->Belt.Array.map(normalizeScalar));
  | `Matrix(mat) =>
    let isNaN = mat.elements->Belt.Array.some(scalarIsNaN);
    isNaN ? `NaN : `Matrix(mat->Matrix.map(normalizeScalar));
  | `NaN => `NaN
  };

let ofInt = (a): value => `Real(Rational(a, 1, Unit))->normalize;

let ofFloat = (v): value =>
  switch (classify_float(v)) {
  | FP_normal
  | FP_subnormal =>
    let magnitude = 1.e6;
    let intMaxF = float_of_int(max_int);
    let numeratorF = v *. magnitude;
    if (abs_float(numeratorF) < intMaxF && FloatUtil.isInt(numeratorF)) {
      let numerator = int_of_float(numeratorF);
      let denominator = int_of_float(magnitude);
      `Real(Real.rational(numerator, denominator, Unit))->normalize;
    } else {
      `Real(Decimal(Decimal.ofFloat(v)))->normalize;
    };
  | FP_zero => `Zero
  | FP_infinite
  | FP_nan => `NaN
  };

let ofStringBase = (base: int, v: string): value => {
  let (withoutMagnitude, magnitudePart) =
    switch (StringUtil.stringSplitOnChar('e', String.lowercase(v))) {
    | [b, m] => (Some(b), Some(m))
    | [b] => (Some(b), Some("0"))
    | _ => (None, None)
    };
  let (integerPart, decimalPart) =
    switch (
      withoutMagnitude->Belt.Option.map(StringUtil.stringSplitOnChar('.'))
    ) {
    | Some([i, d]) => (Some(i), Some(d))
    | Some([i]) => (Some(i), Some("0"))
    | _ => (None, None)
    };
  switch (integerPart, decimalPart, magnitudePart) {
  | (Some(integer), Some(decimal), Some(magnitude)) =>
    // let numer = Z.of_string_base(base, integer ++ decimal);
    // let denom = Z.pow(Z.of_int(base), String.length(decimal));
    // let exponent = QUtil.powInt(10, int_of_string(magnitude));
    // `Real((Q.(make(numer, denom) * exponent), Unit))->normalize;
    failwith("TEST")
  | _ => `NaN
  };
};

let ofDecimal = (f): value => `Real(Decimal(f))->normalize;
let ofString = ofStringBase(10);

let vector2 = (a, b): value => `Vector([|a, b|])->normalize;
let vector3 = (a, b, c): value => `Vector([|a, b, c|])->normalize;
let real = (a: Real.t): value => `Real(a)->normalize;
let imag = (a: Real.t): value => `Imag(a)->normalize;
let complex = (re, im): value => `Complex((re, im))->normalize;
let percent = v => `Percent(v)->normalize;
let vector = (elements): value => `Vector(elements)->normalize;

let matrix = (numRows, numColumns, elements): value =>
  `Matrix(Matrix.{numRows, numColumns, elements});
let matrix2 = (a, b, c, d): value =>
  `Matrix(Matrix.{numRows: 2, numColumns: 2, elements: [|a, b, c, d|]})
  ->normalize;
let matrix3 = (a, b, c, d, e, f, g, h, i): value =>
  `Matrix(
    Matrix.{
      numRows: 3,
      numColumns: 3,
      elements: [|a, b, c, d, e, f, g, h, i|],
    },
  )
  ->normalize;

let valueOfScalar = a => a->valueOfScalar->normalize;
let valueOfMatrix = a => a->valueOfMatrix->normalize;

let toDecimal = (a: value): Decimal.t =>
  switch (a) {
  | `Zero => Decimal.zero
  | `Real(re) => Real.toDecimal(re)
  | _ => Decimal.nan
  };

let toInt = (a: value): option(int) =>
  switch (a) {
  | `Zero => Some(0)
  | `Real(re) =>
    let f = Real.toDecimal(re);
    let floatVal = Decimal.toFloat(f);
    let intVal = int_of_float(floatVal);
    if (float_of_int(intVal) == floatVal) {
      Some(intVal);
    } else {
      None;
    };
  | _ => None
  };