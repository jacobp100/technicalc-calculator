type matrixBase('a) = [
  | `Vector2('a, 'a)
  | `Vector3('a, 'a, 'a)
  | `Matrix2('a, 'a, 'a, 'a)
  | `Matrix3('a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a)
];

let mapAny = (matrix: matrixBase('a), z: 'a => 'b): matrixBase('b) =>
  switch (matrix) {
  | `Vector2(a, b) => `Vector2((z(a), z(b)))
  | `Vector3(a, b, c) => `Vector3((z(a), z(b), z(c)))
  | `Matrix2(a, b, c, d) => `Matrix2((z(a), z(b), z(c), z(d)))
  | `Matrix3(a, b, c, d, e, f, g, h, i) =>
    `Matrix3((z(a), z(b), z(c), z(d), z(e), z(f), z(g), z(h), z(i)))
  };
