open Value_Types;
open Value_Base;

let dot = (a: t, b: t): t =>
  switch (a, b) {
  | (`Vector(aV), `Vector(bV)) => Vector.dot(aV, bV)->ofScalar
  | _ => Value_Arithmetic.mul(a, b)
  };

let abs = (a: t): t =>
  switch (a) {
  | #Scalar.t as t => Scalar.abs(t)->ofScalar
  | `Percent(p) => Scalar.abs(p)->ofPercent
  | `Matrix(m) => Matrix.determinant(m)->ofScalar
  | `Vector(v) => Vector.magnitudeSquared(v)->ofScalar->Value_Powers.sqrt
  | `NaN => `NaN
  };

let%private mapScalar = (a: t, fn: Scalar.t => Scalar.t): t =>
  switch (a) {
  | #Scalar.t as t => fn(t)->ofScalar
  | `Percent(p) => fn(p)->ofPercent
  | `Matrix(m) => Matrix.map(m, fn)->ofMatrix
  | `Vector(v) => Vector.map(v, fn)->ofVector
  | `NaN => `NaN
  };

let round = a => mapScalar(a, Scalar.round);
let floor = a => mapScalar(a, Scalar.floor);
let ceil = a => mapScalar(a, Scalar.ceil);

let%private map2Scalar = (a: t, b: t, fn: (Scalar.t, Scalar.t) => Scalar.t): t =>
  switch (a, b) {
  | (#Scalar.t as a, #Scalar.t as b) => fn(a, b)->ofScalar
  | _ => `NaN
  };

let max = (a, b) => map2Scalar(a, b, Scalar.max);
let min = (a, b) => map2Scalar(a, b, Scalar.min);
let gcd = (a, b) => map2Scalar(a, b, Scalar.gcd);
let lcm = (a, b) => map2Scalar(a, b, Scalar.lcm);
