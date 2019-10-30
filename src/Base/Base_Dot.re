open Types;

let dot = (a: value, b: value): value =>
  switch (a, b) {
  | (`Vector([|a0, b0|]), `Vector([|a1, b1|])) =>
    let (+) = Base_Operators.addScalar;
    let ( * ) = Base_Operators.mulScalar;
    (a0 * a1 + b0 * b1)->valueOfScalar;
  | (`Vector([|a0, b0, c0|]), `Vector([|a1, b1, c1|])) =>
    let (+) = Base_Operators.addScalar;
    let ( * ) = Base_Operators.mulScalar;
    (a0 * a1 + b0 * b1 + c0 * c1)->valueOfScalar;
  | _ => Base_Operators.mul(a, b)
  };