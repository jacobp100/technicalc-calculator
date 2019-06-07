open Types;
open Matrix;

let addTuple = (aQ, aC, bQ, bC) =>
  Constant.(aC == bC) ?
    (Q.(aQ + bQ), aC) : (Q.(QCUtil.toQ(aQ, aC) + QCUtil.toQ(bQ, bC)), Unit);

let subTuple = (aQ, aC, bQ, bC) =>
  Constant.(aC == bC) ?
    (Q.(aQ - bQ), aC) : (Q.(QCUtil.toQ(aQ, aC) - QCUtil.toQ(bQ, bC)), Unit);

let mulTuple = (aQ, aC, bQ, bC) =>
  switch (aC, bC) {
  | (Constant.Unit, _) => (Q.(aQ * bQ), bC)
  | (_, Constant.Unit) => (Q.(aQ * bQ), aC)
  | (Sqrt(aCSqrt), Sqrt(bCSqrt)) => (
      Q.(aQ * bQ),
      Sqrt(Z.(aCSqrt * bCSqrt)),
    )
  | _ => (Q.(QCUtil.toQ(aQ, aC) * QCUtil.toQ(bQ, bC)), Unit)
  };

let divTuple = (aQ, aC, bQ, bC) =>
  switch (aC, bC) {
  | _ when Q.(bQ == Q.zero) => (Q.undef, Constant.Unit)
  | (_, Constant.Unit) => (Q.(aQ / bQ), aC)
  | _ when Constant.(aC == bC) => (Q.(aQ / bQ), Unit)
  | (Constant.Sqrt(aCSqrt), Sqrt(bCSqrt))
      when Z.(aCSqrt > bCSqrt && rem(aCSqrt, bCSqrt) == Z.zero) => (
      Q.(aQ / bQ),
      Sqrt(Z.(aCSqrt / bCSqrt)),
    )
  | _ => (Q.(QCUtil.toQ(aQ, aC) / QCUtil.toQ(bQ, bC)), Unit)
  };

let magnitudeSquaredTuple = (reQ, reC, imQ, imC) => {
  let (reQ, reC) = mulTuple(reQ, reC, reQ, reC);
  let (imQ, imC) = mulTuple(imQ, imC, imQ, imC);
  addTuple(reQ, reC, imQ, imC);
};

let addScalar = (a: scalar, b: scalar): scalar =>
  switch (a, b) {
  | (`Zero, _) => b
  | (_, `Zero) => a
  | (`Real(aQ, aC), `Real(bQ, bC)) =>
    let (q, c) = addTuple(aQ, aC, bQ, bC);
    `Real((q, c));
  | (`Imag(aQ, aC), `Imag(bQ, bC)) =>
    let (q, c) = addTuple(aQ, aC, bQ, bC);
    `Imag((q, c));
  | (`Real(reQ, reC), `Imag(imQ, imC)) => `Complex((reQ, reC, imQ, imC))
  | (`Imag(imQ, imC), `Real(reQ, reC)) => `Complex((reQ, reC, imQ, imC))
  | (`Real(aReQ, aReC), `Complex(bReQ, bReC, imQ, imC))
  | (`Complex(bReQ, bReC, imQ, imC), `Real(aReQ, aReC)) =>
    let (reQ, reC) = addTuple(aReQ, aReC, bReQ, bReC);
    `Complex((reQ, reC, imQ, imC));
  | (`Imag(aImQ, aImC), `Complex(reQ, reC, bImQ, bImC))
  | (`Complex(reQ, reC, bImQ, bImC), `Imag(aImQ, aImC)) =>
    let (imQ, imC) = addTuple(aImQ, aImC, bImQ, bImC);
    `Complex((reQ, reC, imQ, imC));
  | (`Complex(aReQ, aReC, aImQ, aImC), `Complex(bReQ, bReC, bImQ, bImC)) =>
    let (reQ, reC) = addTuple(aReQ, aReC, bReQ, bReC);
    let (imQ, imC) = addTuple(aImQ, aImC, bImQ, bImC);
    `Complex((reQ, reC, imQ, imC));
  };

let subScalar = (a, b) => addScalar(a, Base_Functions.negScalar(b));

let mulScalar = (a: scalar, b: scalar): scalar =>
  switch (a, b) {
  | (`Zero, `Zero)
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_))
  | (`Real(_) | `Imag(_) | `Complex(_), `Zero) => `Zero
  | (`Real(aReQ, aReC), `Real(bReQ, bReC)) =>
    let (reQ, reC) = mulTuple(aReQ, aReC, bReQ, bReC);
    `Real((reQ, reC));
  | (`Imag(aImQ, aImC), `Imag(bImQ, bImC)) =>
    /* CHECK */
    let (reQ, reC) = mulTuple(aImQ, aImC, bImQ, bImC);
    `Real((Q.neg(reQ), reC));
  | (`Real(reQ, reC), `Imag(imQ, imC))
  | (`Imag(imQ, imC), `Real(reQ, reC)) =>
    let (imQ, imC) = mulTuple(reQ, reC, imQ, imC);
    `Imag((imQ, imC));
  | (`Real(aReQ, aReC), `Complex(bReQ, bReC, imQ, imC))
  | (`Complex(bReQ, bReC, imQ, imC), `Real(aReQ, aReC)) =>
    let (reQ, reC) = mulTuple(aReQ, aReC, bReQ, bReC);
    let (imQ, imC) = mulTuple(aReQ, aReC, imQ, imC);
    `Complex((reQ, reC, imQ, imC));
  | (`Imag(aImQ, aImC), `Complex(bReQ, bReC, bImQ, bImC))
  | (`Complex(bReQ, bReC, bImQ, bImC), `Imag(aImQ, aImC)) =>
    let (reQ, reC) = mulTuple(aImQ, aImC, bImQ, bImC);
    let (imQ, imC) = mulTuple(aImQ, aImC, bReQ, bReC);
    `Complex((Q.neg(reQ), reC, imQ, imC));
  | (`Complex(aReQ, aReC, aImQ, aImC), `Complex(bReQ, bReC, bImQ, bImC)) =>
    let (reReQ, reReC) = mulTuple(aReQ, aReC, bReQ, bReC);
    let (imImQ, imImC) = mulTuple(aImQ, aImC, bImQ, bImC);
    let (reImQ, reImC) = mulTuple(aReQ, aReC, bImQ, bImC);
    let (imReQ, imReC) = mulTuple(aImQ, aImC, bReQ, bReC);
    let (reQ, reC) = subTuple(reReQ, reReC, imImQ, imImC);
    let (imQ, imC) = addTuple(reImQ, reImC, imReQ, imReC);
    `Complex((reQ, reC, imQ, imC));
  };

let divScalar = (a: scalar, b: scalar): scalar =>
  switch (a, b) {
  | (_, `Zero) => `Real((Q.undef, Unit))
  | (`Zero, _) => `Zero
  | (`Real(aQ, aC), `Real(bQ, bC)) =>
    let (q, c) = divTuple(aQ, aC, bQ, bC);
    `Real((q, c));
  | (`Imag(aQ, aC), `Real(bQ, bC)) =>
    let (q, c) = divTuple(aQ, aC, bQ, bC);
    `Imag((q, c));
  | (`Real(aQ, aC), `Imag(bQ, bC)) =>
    let (q, c) = divTuple(Q.neg(aQ), aC, bQ, bC);
    `Imag((q, c));
  | (`Imag(aQ, aC), `Imag(bQ, bC)) =>
    let (q, c) = divTuple(aQ, aC, bQ, bC);
    `Real((q, c));
  | (`Complex(aReQ, aReC, aImQ, aImC), `Real(bReQ, bReC)) =>
    let (reQ, reC) = divTuple(aReQ, aReC, bReQ, bReC);
    let (imQ, imC) = divTuple(aImQ, aImC, bReQ, bReC);
    `Complex((reQ, reC, imQ, imC));
  | (`Complex(aReQ, aReC, aImQ, aImC), `Imag(bImQ, bImC)) =>
    let (reQ, reC) = divTuple(aImQ, aImC, bImQ, bImC);
    let (imQ, imC) = divTuple(Q.neg(aReQ), aReC, bImQ, bImC);
    `Complex((reQ, reC, imQ, imC));
  | (`Real(aReQ, aReC), `Complex(bReQ, bReC, bImQ, bImC)) =>
    let (sQ, sC) = magnitudeSquaredTuple(bReQ, bReC, bImQ, bImC);
    let (reQ, reC) = mulTuple(aReQ, aReC, bReQ, bReC);
    let (reQ, reC) = divTuple(reQ, reC, sQ, sC);
    let (imQ, imC) = mulTuple(Q.neg(aReQ), aReC, bImQ, bImC);
    let (imQ, imC) = divTuple(imQ, imC, sQ, sC);
    `Complex((reQ, reC, imQ, imC));
  | (`Imag(aImQ, aImC), `Complex(bReQ, bReC, bImQ, bImC)) =>
    let (sQ, sC) = magnitudeSquaredTuple(bReQ, bReC, bImQ, bImC);
    let (reQ, reC) = mulTuple(aImQ, aImC, bImQ, bImC);
    let (reQ, reC) = divTuple(reQ, reC, sQ, sC);
    let (imQ, imC) = mulTuple(aImQ, aImC, bReQ, bReC);
    let (imQ, imC) = divTuple(imQ, imC, sQ, sC);
    `Complex((reQ, reC, imQ, imC));
  | (`Complex(_), `Complex(bReQ, bReC, bImQ, bImC)) =>
    let (sQ, sC) = magnitudeSquaredTuple(bReQ, bReC, bImQ, bImC);
    let (bRecipReQ, bRecipReC) = divTuple(bReQ, bReC, sQ, sC);
    let (bRecipImQ, bImcipImC) = divTuple(Q.neg(bImQ), bImC, sQ, sC);
    let bRecip = `Complex((bRecipReQ, bRecipReC, bRecipImQ, bImcipImC));
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
    addScalar(aV, mulScalar(aV, p))->valueOfScalar
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
