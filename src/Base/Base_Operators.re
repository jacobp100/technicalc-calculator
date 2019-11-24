open Types;
open Matrix;

let magnitudeSquared = (re, im) => {
  let re = Real.mul(re, re);
  let im = Real.mul(im, im);
  Real.add(re, im);
};

let addScalar = (a: scalar, b: scalar): scalar =>
  switch (a, b) {
  | (`Zero, _) => b
  | (_, `Zero) => a
  | (`Real(a), `Real(b)) => `Real(Real.add(a, b))
  | (`Imag(a), `Imag(b)) => `Imag(Real.add(a, b))
  | (`Real(re), `Imag(im))
  | (`Imag(im), `Real(re)) => `Complex((re, im))
  | (`Real(aRe), `Complex(bRe, im))
  | (`Complex(bRe, im), `Real(aRe)) => `Complex((Real.add(aRe, bRe), im))
  | (`Imag(aIm), `Complex(re, bIm))
  | (`Complex(re, bIm), `Imag(aIm)) => `Complex((re, Real.add(aIm, bIm)))
  | (`Complex(aRe, aIm), `Complex(bRe, bIm)) =>
    `Complex((Real.add(aRe, bRe), Real.add(aIm, bIm)))
  };

let subScalar = (a, b) => addScalar(a, Base_Functions.negScalar(b));

let mulScalar = (a: scalar, b: scalar): scalar =>
  switch (a, b) {
  | (`Zero, `Zero)
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_))
  | (`Real(_) | `Imag(_) | `Complex(_), `Zero) => `Zero
  | (`Real(a), `Real(b)) => `Real(Real.mul(a, b))
  | (`Imag(a), `Imag(b)) => `Real(Real.mul(a, b)->Real.neg)
  | (`Real(re), `Imag(im))
  | (`Imag(im), `Real(re)) => `Imag(Real.mul(re, im))
  | (`Real(aRe), `Complex(bRe, bIm))
  | (`Complex(bRe, bIm), `Real(aRe)) =>
    let re = Real.mul(aRe, bRe);
    let im = Real.mul(aRe, bIm);
    `Complex((re, im));
  | (`Imag(aIm), `Complex(bRe, bIm))
  | (`Complex(bRe, bIm), `Imag(aIm)) =>
    let re = Real.mul(aIm, bIm);
    let im = Real.mul(aIm, bRe);
    `Complex((Real.neg(re), im));
  | (`Complex(aRe, aIm), `Complex(bRe, bIm)) =>
    let reRe = Real.mul(aRe, bRe);
    let imIm = Real.mul(aIm, bIm);
    let reIm = Real.mul(aRe, bIm);
    let imRe = Real.mul(aIm, bRe);
    let reQ = Real.sub(reRe, imIm);
    let imQ = Real.add(reIm, imRe);
    `Complex((reQ, imQ));
  };

let divScalar = (a: scalar, b: scalar): scalar =>
  switch (a, b) {
  | (_, `Zero) => `Real(Real.nan)
  | (`Zero, _) => `Zero
  | (`Real(a), `Real(b)) => `Real(Real.div(a, b))
  | (`Imag(im), `Real(re)) => `Imag(Real.div(im, re))
  | (`Real(re), `Imag(im)) => `Imag(Real.div(Real.neg(re), im))
  | (`Imag(a), `Imag(b)) => `Real(Real.div(a, b))
  | (`Complex(aRe, aIm), `Real(bRe)) =>
    let re = Real.div(aRe, bRe);
    let im = Real.div(aIm, bRe);
    `Complex((re, im));
  | (`Complex(aRe, aIm), `Imag(bIm)) =>
    let re = Real.div(aIm, bIm);
    let im = Real.div(Real.neg(aRe), bIm);
    `Complex((re, im));
  | (`Real(aRe), `Complex(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let re = Real.mul(aRe, bRe)->Real.div(s);
    let im = Real.mul(Real.neg(aRe), bIm)->Real.div(s);
    `Complex((re, im));
  | (`Imag(aIm), `Complex(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let re = Real.mul(aIm, bIm)->Real.div(s);
    let im = Real.mul(aIm, bRe)->Real.div(s);
    `Complex((re, im));
  | (`Complex(_), `Complex(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let bRecipRe = Real.div(bRe, s);
    let bRecipIm = Real.div(Real.neg(bIm), s);
    let bRecip = `Complex((bRecipRe, bRecipIm));
    mulScalar(a, bRecip);
  };

let add = (a: value, b: value): value =>
  switch (a, b) {
  | (
      `Zero | `Real(_) | `Imag(_) | `Complex(_) | `Vector(_) | `Matrix(_),
      `Zero,
    ) => a
  | (
      `Zero,
      `Real(_) | `Imag(_) | `Complex(_) | `Vector(_) | `Matrix(_) | `Percent(_),
    ) => b
  | (
      (`Real(_) | `Imag(_) | `Complex(_)) as aV,
      (`Real(_) | `Imag(_) | `Complex(_)) as bV,
    ) =>
    addScalar(aV, bV)->valueOfScalar
  | ((`Real(_) | `Imag(_) | `Complex(_)) as aV, `Percent(p)) =>
    addScalar(aV, mulScalar(aV, p)->divScalar(`Real(Real.ofInt(100))))
    ->valueOfScalar
  | (`Vector(aElements), `Vector(bElements))
      when Belt.Array.length(aElements) == Belt.Array.length(bElements) =>
    `Vector(Belt.Array.zipBy(aElements, bElements, addScalar))
  | (`Matrix(aM), `Matrix(bM)) =>
    switch (Matrix.zipBy(aM, bM, addScalar)) {
    | Some(m) => `Matrix(m)
    | None => `NaN
    }
  | _ => `NaN
  };

let sub = (a, b) => add(a, Base_Functions.neg(b));

let mul = (a: value, b: value): value =>
  switch (a, b) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_), `Zero)
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_)) => zero
  | (
      (`Real(_) | `Imag(_) | `Complex(_)) as aV,
      (`Real(_) | `Imag(_) | `Complex(_)) as bV,
    ) =>
    mulScalar(aV, bV)->valueOfScalar
  | (`Percent(p), (`Real(_) | `Imag(_) | `Complex(_)) as v)
  | ((`Real(_) | `Imag(_) | `Complex(_)) as v, `Percent(p)) =>
    `Percent(mulScalar(p, v))
  | (`Vector(v), (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as value)
  | ((`Zero | `Real(_) | `Imag(_) | `Complex(_)) as value, `Vector(v)) =>
    `Vector(v->Belt.Array.map(mulScalar(value)))
  | (`Matrix(m), (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as value)
  | ((`Zero | `Real(_) | `Imag(_) | `Complex(_)) as value, `Matrix(m)) =>
    `Matrix(m->Matrix.map(mulScalar(value)))
  | (`Vector([|a1, a2, a3|]), `Vector([|b1, b2, b3|])) =>
    let (-) = subScalar;
    let ( * ) = mulScalar;
    vector3(a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1);
  | (`Matrix(aM), `Matrix(bM)) when aM.numColumns == bM.numRows =>
    let shape = aM.numColumns;
    let m =
      Matrix.makeBy(
        shape,
        shape,
        (row, column) => {
          let element = ref(`Zero);
          for (i in 0 to shape - 1) {
            let elementProduct =
              mulScalar(
                Matrix.getExn(aM, row, i),
                Matrix.getExn(bM, i, column),
              );
            element := addScalar(element^, elementProduct);
          };
          element^;
        },
      );
    `Matrix(m);
  | (
      `Matrix({numColumns: 1, numRows: 2, elements: [|a1, a2, a3, a4|]}),
      `Vector([|b1, b2|]),
    ) =>
    let (+) = addScalar;
    let ( * ) = mulScalar;
    `Vector([|a1 * b1 + a2 * b2, a3 * b1 + a4 * b2|]);
  | (
      `Matrix({
        numColumns: 1,
        numRows: 3,
        elements: [|a1, a2, a3, a4, a5, a6, a7, a8, a9|],
      }),
      `Vector([|b1, b2, b3|]),
    ) =>
    let (+) = addScalar;
    let ( * ) = mulScalar;
    `Vector([|
      a1 * b1 + a2 * b2 + a3 * b3,
      a4 * b1 + a5 * b2 + a6 * b3,
      a7 * b1 + a8 * b2 + a9 * b3,
    |]);
  | _ => `NaN
  };

let div = (a: value, b: value): value =>
  switch (a, b) {
  | (_, `Zero) => `NaN
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_)) => `Zero
  | (
      (`Real(_) | `Imag(_) | `Complex(_)) as aV,
      (`Real(_) | `Imag(_) | `Complex(_)) as bV,
    ) =>
    divScalar(aV, bV)->valueOfScalar
  | (`Percent(p), (`Real(_) | `Imag(_) | `Complex(_)) as v) =>
    `Percent(divScalar(p, v))
  | (`Vector(v), (`Real(_) | `Imag(_) | `Complex(_)) as value) =>
    `Vector(Belt.Array.map(v, divScalar(_, value)))
  | (`Matrix(m), (`Real(_) | `Imag(_) | `Complex(_)) as value) =>
    `Matrix(Matrix.map(m, divScalar(_, value)))
  | _ => `NaN
  };