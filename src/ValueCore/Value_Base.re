open Value_Types;

external ofPercentUnsafe: percent => t = "%identity";
external ofScalarUnsafe: Scalar.t => t = "%identity";
external ofMatrixUnsafe: matrix => t = "%identity";
external ofVectorUnsafe: vector => t = "%identity";

let zero: t = `Zero;
let one: t = `Real(Rational(1, 1, Unit));
let minusOne: t = `Real(Rational(-1, 1, Unit));
let i: t = `Imag(Rational(1, 1, Unit));
let minusI: t = `Imag(Rational(-1, 1, Unit));
let pi: t = `Real(Rational(1, 1, Pi));
let e: t = `Real(Rational(1, 1, Exp(1)));
let nan: t = `NaN;

let equal = (a: t, b: t): bool =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.equal(aS, bS)
  | (`Percent(aP), `Percent(bP)) => Scalar.equal(aP, bP)
  | (`Vector(aV), `Vector(bV)) => Vector.equal(aV, bV)
  | (`Matrix(aM), `Matrix(bM)) => Matrix.equal(aM, bM)
  | _ => false
  };

let normalize = (a: t): t =>
  switch (a) {
  | `Zero => `Zero
  | (`Real(v) | `Imag(v)) as scalar =>
    Real.isNaN(v) ? `NaN : Scalar.normalize(scalar)->ofScalarUnsafe
  | `Complex(re, im) as scalar =>
    Real.isNaN(re) || Real.isNaN(im)
      ? `NaN : Scalar.normalize(scalar)->ofScalarUnsafe
  | `Percent(p) =>
    if (Scalar.isNaN(p)) {
      `NaN;
    } else {
      let normalized = Scalar.normalize(p);
      normalized === p ? a : `Percent(Scalar.normalize(p));
    }
  | `Vector(vector) =>
    Vector.isEmpty(vector) || Vector.some(vector, Scalar.isNaN)
      ? `NaN : `Vector(Vector.map(vector, Scalar.normalize))
  | `Matrix(matrix) =>
    Matrix.isEmpty(matrix) || Matrix.some(matrix, Scalar.isNaN)
      ? `NaN : `Matrix(Matrix.map(matrix, Scalar.normalize))
  | `NaN => `NaN
  };

let ofScalar = (a: Scalar.t): t => ofScalarUnsafe(a)->normalize;
let ofReal = (a: Real.t): t => normalize(`Real(a));
let ofImag = (a: Real.t): t => normalize(`Imag(a));
let ofComplex = (re: Real.t, im: Real.t): t =>
  normalize(`Complex((re, im)));
let ofPercent = (a: Scalar.t): t => normalize(`Percent(a));
let ofVector = (a: Vector.t): t => normalize(`Vector(a));
let ofMatrix = (a: Matrix.t): t => normalize(`Matrix(a));

let ofDecimal = (a): t => `Real(Decimal(a))->normalize;
let ofInt = (a): t => `Real(Real.ofInt(a))->normalize;
let ofFloat = (a): t => {
  let scalar = Scalar.ofFloat(a);
  Scalar.isNaN(scalar) ? `NaN : ofScalarUnsafe(scalar);
};

let toDecimal = (a: t): Decimal.t =>
  switch (a) {
  | #Scalar.t as s => Scalar.toDecimal(s)
  | _ => Decimal.nan
  };

let toInt = (a: t): option(int) =>
  switch (a) {
  | #Scalar.t as s => Scalar.toInt(s)
  | _ => None
  };

let toFloat = (a: t): float =>
  switch (a) {
  | #Scalar.t as s => Scalar.toFloat(s)
  | _ => Pervasives.nan
  };
