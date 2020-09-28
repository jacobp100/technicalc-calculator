open Value_Types;
open Value_Base;

let%private hundredS: Scalar.t = `Real(Real.ofInt(100));

let add = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.add(aS, bS)->ofScalar
  | (#Scalar.t as s, `Percent(p)) => Scalar.(s + s * p / hundredS)->ofScalar
  | (`Vector(aV), `Vector(bV)) => Vector.add(aV, bV)->ofVector
  | (`Matrix(aM), `Matrix(bM)) => Matrix.add(aM, bM)->ofMatrix
  | _ => `NaN
  };

let sub = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.sub(aS, bS)->ofScalar
  | (#Scalar.t as s, `Percent(p)) => Scalar.(s - s * p / hundredS)->ofScalar
  | (`Vector(aV), `Vector(bV)) => Vector.sub(aV, bV)->ofVector
  | (`Matrix(aM), `Matrix(bM)) => Matrix.sub(aM, bM)->ofMatrix
  | _ => `NaN
  };

let mul = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.mul(aS, bS)->ofScalar
  | (#Scalar.t as s, `Percent(p))
  | (`Percent(p), #Scalar.t as s) => Scalar.(p * s / hundredS)->ofScalar
  | (`Vector(aV), `Vector(bV)) => Vector.mul(aV, bV)->ofVector
  | (`Matrix(aM), `Matrix(bM)) => Matrix.mul(aM, bM)->ofMatrix
  | (`Vector(v), #Scalar.t as s)
  | (#Scalar.t as s, `Vector(v)) => Vector.mulScalar(v, s)->ofVector
  | (`Matrix(m), #Scalar.t as s)
  | (#Scalar.t as s, `Matrix(m)) => Matrix.mulScalar(m, s)->ofMatrix
  | (`Matrix(m), `Vector(v)) => Matrix.mulVector(m, v)->ofVector
  | _ => `NaN
  };

let div = (a: t, b: t) =>
  switch (a, b) {
  | (#Scalar.t as aS, #Scalar.t as bS) => Scalar.div(aS, bS)->ofScalar
  | (#Scalar.t as s, `Percent(p)) =>
    Scalar.(s / (one + p / hundredS))->ofScalar
  | (`Vector(v), #Scalar.t as s) => Vector.divScalar(v, s)->ofVector
  | (`Matrix(m), #Scalar.t as s) => Matrix.divScalar(m, s)->ofMatrix
  | _ => `NaN
  };
