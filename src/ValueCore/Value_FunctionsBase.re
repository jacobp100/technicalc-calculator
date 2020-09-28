open Value_Types;
open Value_Base;

let neg = (a: t) =>
  switch (a) {
  | #Scalar.t as s => Scalar.neg(s)->ofScalar
  | `Percent(p) => Scalar.neg(p)->ofPercent
  | `Vector(v) => Vector.neg(v)->ofVector
  | `Matrix(m) => Matrix.neg(m)->ofMatrix
  | `NaN => `NaN
  };
