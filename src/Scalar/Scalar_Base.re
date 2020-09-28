open Scalar_Types;

let one: t = `Real(Real.one);
let minusOne: t = `Real(Real.minusOne);
let zero: t = `Real(Real.zero);
let nan: t = `Real(Real.nan);

let normalize = (v: t): t =>
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

let isNaN = (a: t) =>
  switch (a) {
  | `Zero => false
  | `Real(v)
  | `Imag(v) => Real.isNaN(v)
  | `Complex(re, im) => Real.isNaN(re) || Real.isNaN(im)
  };

let ofFloat = (v): t =>
  switch (classify_float(v)) {
  | FP_normal
  | FP_subnormal =>
    let magnitude = 1.e6;
    let intMaxF = float_of_int(max_int);
    let numeratorF = v *. magnitude;
    switch (FloatUtil.intValue(numeratorF), FloatUtil.intValue(magnitude)) {
    | (Some(numerator), Some(denominator))
        when abs_float(numeratorF) < intMaxF =>
      `Real(Real.rational(numerator, denominator, Unit))->normalize
    | _ => `Real(Real.decimal(Decimal.ofFloat(v)))->normalize
    };
  | FP_zero => `Zero
  | FP_infinite
  | FP_nan => nan
  };

let toDecimal = (a: t): Decimal.t =>
  switch (a) {
  | `Zero => Decimal.zero
  | `Real(re) => Real.toDecimal(re)
  | _ => Decimal.nan
  };

let toInt = (a: t): option(int) =>
  switch (a) {
  | `Zero => Some(0)
  | `Real(re) => Real.toDecimal(re)->Decimal.toFloat->FloatUtil.intValue
  | _ => None
  };

let toFloat = (a: t): float =>
  switch (a) {
  | `Zero => 0.
  | `Real(re) => Real.toDecimal(re)->Decimal.toFloat
  | _ => Pervasives.nan
  };

let equal = (a: t, b: t): bool =>
  switch (a, b) {
  | (`Zero, `Zero) => true
  | (`Real(a), `Real(b))
  | (`Imag(a), `Imag(b)) => Real.(a == b)
  | (`Complex(aRe, aIm), `Complex(bRe, bIm)) =>
    Real.(aRe == bRe && aIm == bIm)
  | _ => false
  };

let map = (a: t, f: Real.t => Real.t): t =>
  switch (a) {
  | `Zero => `Zero
  | `Real(re) => `Real(f(re))
  | `Imag(im) => `Imag(f(im))
  | `Complex(re, im) => `Complex((f(re), f(im)))
  };
