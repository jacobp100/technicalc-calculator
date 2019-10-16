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
/* let round = mapQValue(_, roundScalar)*/
// let negScalar = mapQScalar(_, Q.neg);
// let absScalar = mapQScalar(_, Q.abs);
// let floorScalar = mapQScalar(_, q => QUtil.floor(q)->Q.of_bigint);
// let ceilScalar = mapQScalar(_, q => QUtil.ceil(q)->Q.of_bigint);
// let roundScalar = mapQScalar(_, q => QUtil.round(q)->Q.of_bigint);

let mapRealValue = (a: value, fn: scalar => scalar) =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV => fn(aV)->valueOfScalar
  | `Percent(p) => `Percent(fn(p))
  | `Vector(elements) => `Vector(elements->Belt.Array.map(fn))
  | `Matrix(elements) => `Matrix(elements->Matrix.map(fn))
  | `NaN => `NaN
  };
let neg = mapRealValue(_, negScalar);
let abs = mapRealValue(_, absScalar);

// let floor = mapQValue(_, floorScalar);
/* let ceil = mapQValue(_, ceilScalar)*/