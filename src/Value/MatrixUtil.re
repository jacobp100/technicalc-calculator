open Types;
open MatrixTypes;

let map = (matrix: matrix, fn: scalar => scalar): value =>
  matrix->mapAny(fn)->valueOfMatrix;

let coerceScalar = (v: value): scalar =>
  switch (v) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV => aV
  | _ => `Real((Q.undef, Constant.Unit))
  };

let flatMap = (matrix: matrixBase('a), fn: 'a => value): value =>
  matrix->mapAny(v => fn(v)->coerceScalar)->valueOfMatrix;

let map2 = (x, y, z: (scalar, scalar) => scalar): value =>
  switch (x, y) {
  | (`Vector2(a0, b0), `Vector2(a1, b1)) => vector2(z(a0, a1), z(b0, b1))
  | (`Vector3(a0, b0, c0), `Vector3(a1, b1, c1)) =>
    vector3(z(a0, a1), z(b0, b1), z(c0, c1))
  | (`Matrix2(a0, b0, c0, d0), `Matrix2(a1, b1, c1, d1)) =>
    matrix2(z(a0, a1), z(b0, b1), z(c0, c1), z(d0, d1))
  | (
      `Matrix3(a0, b0, c0, d0, e0, f0, g0, h0, i0),
      `Matrix3(a1, b1, c1, d1, e1, f1, g1, h1, i1),
    ) =>
    let (m00, m01, m02) = (z(a0, a1), z(b0, b1), z(c0, c1));
    let (m10, m11, m12) = (z(d0, d1), z(e0, e1), z(f0, f1));
    let (m20, m21, m22) = (z(g0, g1), z(h0, h1), z(i0, i1));
    matrix3(m00, m01, m02, m10, m11, m12, m20, m21, m22);
  | (
      `Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_),
      `Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_),
    ) => `NaN
  };

let toIdentity = (x: value): value => {
  let zero: scalar = `Real((Q.zero, Unit));
  let one: scalar = `Real((Q.one, Unit));
  switch (x) {
  | `Matrix2(_) => matrix2(one, zero, zero, one)
  | `Matrix3(_) => matrix3(one, zero, zero, zero, one, zero, zero, zero, one)
  | _ => `NaN
  };
};
