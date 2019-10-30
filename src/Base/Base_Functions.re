open Types;

let mapScalar = (a: scalar, f: Real.t => Real.t): scalar =>
  switch (a) {
  | `Zero => `Zero
  | `Real(re) => `Real(f(re))
  | `Imag(im) => `Imag(f(im))
  | `Complex(re, im) => `Complex((f(re), f(im)))
  };

let negScalar = mapScalar(_, Real.neg);
let absScalar = mapScalar(_, Real.abs);
let roundScalar = mapScalar(_, Real.round);
let floorScalar = mapScalar(_, Real.floor);
let ceilScalar = mapScalar(_, Real.ceil);

let mapCompositeValue = (a: value, fn: scalar => scalar) =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV => fn(aV)->valueOfScalar
  | `Percent(p) => `Percent(fn(p))
  | `Vector(elements) => `Vector(elements->Belt.Array.map(fn))
  | `Matrix(elements) => `Matrix(elements->Matrix.map(fn))
  | `NaN => `NaN
  };
let neg = mapCompositeValue(_, negScalar);
let abs = mapCompositeValue(_, absScalar);
let round = mapCompositeValue(_, roundScalar);
let floor = mapCompositeValue(_, floorScalar);
let ceil = mapCompositeValue(_, ceilScalar);