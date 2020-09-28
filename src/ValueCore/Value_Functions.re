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
