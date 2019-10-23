open Types;

let rec re = (a: value): value =>
  switch (a) {
  | `Zero
  | `Imag(_) => `Zero
  | `Real(_) => a
  | `Complex(re, _) => real(re)
  | `Percent(p) => re(Base.percentToNumerical(p))
  | _ => `NaN
  };

let rec im = (a: value): value =>
  switch (a) {
  | `Zero
  | `Real(_) => `Zero
  | `Imag(_) => a
  | `Complex(_, im) => imag(im)
  | `Percent(p) => im(Base.percentToNumerical(p))
  | _ => `NaN
  };

let conjScalar = (a: scalar): scalar =>
  switch (a) {
  | `Zero
  | `Real(_) => a
  | `Imag(im) => `Imag(Real.neg(im))
  | `Complex(re, im) => `Complex((re, Real.neg(im)))
  };

let conj = (a: value): value =>
  switch (a) {
  | `Zero => `Zero
  | `Real(_) => a
  | (`Imag(_) | `Complex(_)) as aS => conjScalar(aS)->valueOfScalar
  | `Percent(p) => `Percent(conjScalar(p))
  | `Vector(elements) => `Vector(elements->Belt.Array.map(conjScalar))
  | `Matrix(m) => `Matrix(m->Matrix.map(conjScalar))
  | `NaN => `NaN
  };