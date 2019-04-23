open Types;

let dot = (a: value, b: value): value =>
  switch (a, b) {
  | (`Vector2(a0, b0), `Vector2(a1, b1)) =>
    let (+) = BasicMath.addScalar;
    let ( * ) = BasicMath.mulScalar;
    (a0 * a1 + b0 * b1)->valueOfScalar;
  | (`Vector3(a0, b0, c0), `Vector3(a1, b1, c1)) =>
    let (+) = BasicMath.addScalar;
    let ( * ) = BasicMath.mulScalar;
    (a0 * a1 + b0 * b1 + c0 * c1)->valueOfScalar;
  | _ => BasicMath.(a * b)
  };
