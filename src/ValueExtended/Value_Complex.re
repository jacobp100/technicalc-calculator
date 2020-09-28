open Value_Core;

let rec re = (a: t): t =>
  switch (a) {
  | `Zero
  | `Imag(_) => `Zero
  | `Real(_) => a
  | `Complex(re, _) => ofReal(re)
  | `Percent(p) => re(Value_Util.percentToNumerical(p))
  | _ => `NaN
  };

let rec im = (a: t): t =>
  switch (a) {
  | `Zero
  | `Real(_) => `Zero
  | `Imag(_) => a
  | `Complex(_, im) => ofImag(im)
  | `Percent(p) => im(Value_Util.percentToNumerical(p))
  | _ => `NaN
  };

let conj = (a: t): t =>
  switch (a) {
  | #Scalar.t as aS => Scalar.conj(aS)->ofScalar
  | `Percent(p) => Scalar.conj(p)->ofPercent
  | `Vector(elements) => `Vector(elements->Belt.Array.map(Scalar.conj))
  | `Matrix(m) => `Matrix(Matrix.map(m, Scalar.conj))
  | `NaN => `NaN
  };
