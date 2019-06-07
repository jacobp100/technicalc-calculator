open Types;
open Matrix;

let abs = a =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV =>
    Base_Functions.absScalar(aV)->valueOfScalar
  | `Percent(p) => `Percent(Base_Functions.absScalar(p))
  | `Matrix({numRows: 2, numColumns: 2, elements: [|a, b, c, d|]}) =>
    let (-) = Base_Operators.subScalar;
    let ( * ) = Base_Operators.mulScalar;
    (a * d - b * c)->valueOfScalar;
  | `Matrix({
      numRows: 3,
      numColumns: 3,
      elements: [|a, b, c, d, e, f, g, h, i|],
    }) =>
    let (+) = Base_Operators.addScalar;
    let (-) = Base_Operators.subScalar;
    let ( * ) = Base_Operators.mulScalar;
    /* https://www.wolframalpha.com/input/?i=det(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D) */
    (a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g)
    ->valueOfScalar;
  | `Vector([|a, b|]) =>
    let (+) = Base_Operators.addScalar;
    let ( * ) = Base_Operators.mulScalar;
    (a * a + b * b)->valueOfScalar;
  | `Vector([|a, b, c|]) =>
    let (+) = Base_Operators.addScalar;
    let ( * ) = Base_Operators.mulScalar;
    (a * a + b * b + c * c)->valueOfScalar;
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };
