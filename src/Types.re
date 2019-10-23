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

let simplifyConstant = (v: scalar): scalar =>
  switch (v) {
  | _ => v
  // | `Zero => `Zero
  // | `Real(q, c) =>
  //   switch (Constant.simplify(c)) {
  //   | `Factor(f, c) => `Real((Q.(f * q), c))
  //   | `Zero => `Zero
  //   | `None => v
  //   }
  // | `Imag(q, c) =>
  //   switch (Constant.simplify(c)) {
  //   | `Factor(f, c) => `Imag((Q.(f * q), c))
  //   | `Zero => `Zero
  //   | `None => v
  //   }
  // | `Complex(reQ, reC, imQ, imC) =>
  //   switch (Constant.simplify(reC), Constant.simplify(imC)) {
  //   | (`None, `None) => v
  //   | (`Factor(reF, reC), `None) => `Complex((Q.(reF * reQ), reC, imQ, imC))
  //   | (`None, `Factor(imF, imC)) => `Complex((reQ, reC, Q.(imF * imQ), imC))
  //   | (`Factor(reF, reC), `Factor(imF, imC)) =>
  //     `Complex((Q.(reF * reQ), reC, Q.(imQ * imF), imC))
  //   | (`Zero, `Zero) => `Zero
  //   | (`Zero, `None) => `Imag((imQ, imC))
  //   | (`None, `Zero) => `Real((reQ, reC))
  //   | (`Zero, `Factor(imF, imC)) => `Imag((Q.(imF * imQ), imC))
  //   | (`Factor(reF, reC), `Zero) => `Real((Q.(reF * reQ), reC))
  //   }
  };

let normalizeScalar = (v: scalar): scalar =>
  switch (v) {
  | _ => v
  // | `Zero => `Zero
  // | `Real(q, _)
  // | `Imag(q, _) => Q.(q == zero) ? `Zero : v->simplifyConstant
  // | `Complex(reQ, reC, imQ, imC) =>
  //   switch (Q.(reQ == zero, imQ == zero)) {
  //   | (true, true) => `Zero
  //   | (false, true) => `Real((reQ, reC))->simplifyConstant
  //   | (true, false) => `Imag((imQ, imC))->simplifyConstant
  //   | (false, false) => v->simplifyConstant
  //   }
  };

let normalize = (v: value): value =>
  switch (v) {
  | _ => v
  // | `Zero => `Zero
  // | (`Real(q, _) | `Imag(q, _)) as scalar =>
  //   qIsNaN(q) ? `NaN : normalizeScalar(scalar)->valueOfScalar
  // | `Complex(reQ, _, imQ, _) as scalar =>
  //   qIsNaN(reQ) || qIsNaN(imQ)
  //     ? `NaN : normalizeScalar(scalar)->valueOfScalar
  // | `Percent(p) =>
  //   if (scalarIsNaN(p)) {
  //     `NaN;
  //   } else {
  //     let normalized = normalizeScalar(p);
  //     normalized === p ? v : `Percent(normalizeScalar(p));
  //   }
  // | `Vector(elements) =>
  //   let isNaN = elements->Belt.Array.some(scalarIsNaN);
  //   isNaN ? `NaN : `Vector(elements->Belt.Array.map(normalizeScalar));
  // | `Matrix(mat) =>
  //   let isNaN = mat.elements->Belt.Array.some(scalarIsNaN);
  //   isNaN ? `NaN : `Matrix(mat->Matrix.map(normalizeScalar));
  // | `NaN => `NaN
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
      `Real(Rational(numerator, denominator, Unit))->normalize;
    } else {
      `Real(Float(v))->normalize;
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

let ofString = ofStringBase(10) /*   }*/;

let vector2 = (a, b): value => `Vector([|a, b|])->normalize;
let vector3 = (a, b, c): value => `Vector([|a, b, c|])->normalize /*   | _ => Pervasives.na*/;

let real = (a: Real.t): value => `Real(a)->normalize;
let imag = (a: Real.t): value => `Imag(a)->normalize;
let complex = (re, im): value => `Complex((re, im))->normalize /* let toFloat = (a: value): float =*/;
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
  ->normalize /*   }*/ /*   | _ => Non*/;

// let valueOfScalar = a => a->valueOfScalar->normalize;
// let valueOfMatrix = a => a->valueOfMatrix->normalize;

let toFloat = (a: value): float =>
  switch (a) {
  | `Zero => 0.
  | `Real(re) => Real.toFloat(re)
  | _ => Pervasives.nan
  };

let toInt = (a: value): option(int) =>
  switch (a) {
  | `Zero => Some(0)
  | `Real(re) =>
    let float = Real.toFloat(re);
    let intF = int_of_float(float);
    if (float_of_int(intF) == float) {
      Some(intF);
    } else {
      None;
    };
  | _ => None
  };