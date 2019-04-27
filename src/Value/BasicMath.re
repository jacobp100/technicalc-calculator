open Types;

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

let mapQScalar = (a: scalar, f: Q.t => Q.t): scalar =>
  switch (a) {
  | `Zero => `Zero
  | `Real(aQ, aC) => `Real((f(aQ), aC))
  | `Imag(aQ, aC) => `Imag((f(aQ), aC))
  | `Complex(reQ, reC, imQ, imC) => `Complex((f(reQ), reC, f(imQ), imC))
  };

let negScalar = mapQScalar(_, Q.neg);

let absScalar = mapQScalar(_, Q.abs);

let floorScalar = mapQScalar(_, q => QUtil.floor(q)->Q.of_bigint);

let ceilScalar = mapQScalar(_, q => QUtil.ceil(q)->Q.of_bigint);

let roundScalar = mapQScalar(_, q => QUtil.round(q)->Q.of_bigint);

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

let subScalar = (a, b) => addScalar(a, negScalar(b));

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

let equalScalar = (a: scalar, b: scalar): bool =>
  switch (a, b) {
  | (`Zero, `Zero) => true
  | (`Real(aQ, aC), `Real(bQ, bC))
  | (`Imag(aQ, aC), `Imag(bQ, bC)) => Q.(aQ == bQ) && Constant.(aC == bC)
  | (`Complex(aReQ, aReC, aImQ, aImC), `Complex(bReQ, bReC, bImQ, bImC)) =>
    Q.(aReQ == bReQ && aImQ == bImQ) && Constant.(aReC == bReC && aImC == bImC)
  | _ => false
  };

let mapQValue = (a: value, fn: scalar => scalar) =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV => fn(aV)->valueOfScalar
  | (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as aM =>
    MatrixUtil.map(aM, fn)
  | `NaN => `NaN
  };

let neg = mapQValue(_, negScalar);

let abs = a =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV =>
    absScalar(aV)->valueOfScalar
  | `Matrix2(a, b, c, d) =>
    let (-) = subScalar;
    let ( * ) = mulScalar;
    (a * d - b * c)->valueOfScalar;
  | `Matrix3(a, b, c, d, e, f, g, h, i) =>
    let (+) = addScalar;
    let (-) = subScalar;
    let ( * ) = mulScalar;
    /* https://www.wolframalpha.com/input/?i=det(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D) */
    (a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g)
    ->valueOfScalar;
  | `Vector2(a, b) =>
    let (+) = addScalar;
    let ( * ) = mulScalar;
    (a * a + b * b)->valueOfScalar;
  | `Vector3(a, b, c) =>
    let (+) = addScalar;
    let ( * ) = mulScalar;
    (a * a + b * b + c * c)->valueOfScalar;
  | `NaN => `NaN
  };

let floor = mapQValue(_, floorScalar);

let ceil = mapQValue(_, ceilScalar);

let round = mapQValue(_, roundScalar);

let add = (a: value, b: value): value =>
  switch (a, b) {
  | (
      `Zero | `Real(_) | `Imag(_) | `Complex(_) | `Vector2(_) | `Vector3(_) |
      `Matrix2(_) |
      `Matrix3(_),
      `Zero,
    ) => a
  | (
      `Zero,
      `Real(_) | `Imag(_) | `Complex(_) | `Vector2(_) | `Vector3(_) |
      `Matrix2(_) |
      `Matrix3(_),
    ) => b
  | (
      (`Real(_) | `Imag(_) | `Complex(_)) as aV,
      (`Real(_) | `Imag(_) | `Complex(_)) as bV,
    ) =>
    addScalar(aV, bV)->valueOfScalar
  | (
      (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as aM,
      (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as bM,
    ) =>
    MatrixUtil.map2(aM, bM, addScalar)
  | _ => `NaN
  };

let sub = (a, b) => add(a, neg(b));

let mul = (a: value, b: value): value =>
  switch (a, b) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_), `Zero)
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_)) => zero
  | (
      (`Real(_) | `Imag(_) | `Complex(_)) as aV,
      (`Real(_) | `Imag(_) | `Complex(_)) as bV,
    ) =>
    mulScalar(aV, bV)->valueOfScalar
  | (
      (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as m,
      (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as v,
    )
  | (
      (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as v,
      (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as m,
    ) =>
    MatrixUtil.map(m, mulScalar(v))
  | (`Vector3(a1, a2, a3), `Vector3(b1, b2, b3)) =>
    let (-) = subScalar;
    let ( * ) = mulScalar;
    vector3(a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1);
  | (`Matrix2(a0, b0, c0, d0), `Matrix2(a1, b1, c1, d1)) =>
    let (+) = addScalar;
    let ( * ) = mulScalar;
    matrix2(
      a0 * a1 + b0 * c1,
      a0 * b1 + b0 * d1,
      c0 * a1 + d0 * c1,
      c0 * b1 + d0 * d1,
    );
  | (
      `Matrix3(a0, b0, c0, d0, e0, f0, g0, h0, i0),
      `Matrix3(a1, b1, c1, d1, e1, f1, g1, h1, i1),
    ) =>
    let (+) = addScalar;
    let ( * ) = mulScalar;
    matrix3(
      a0 * a1 + b0 * d1 + c0 * g1,
      a0 * b1 + b0 * e1 + c0 * h1,
      a0 * c1 + b0 * f1 + c0 * i1,
      d0 * a1 + e0 * d1 + f0 * g1,
      d0 * b1 + e0 * e1 + f0 * h1,
      d0 * c1 + e0 * f1 + f0 * i1,
      g0 * a1 + h0 * d1 + i0 * g1,
      g0 * b1 + h0 * e1 + i0 * h1,
      g0 * c1 + h0 * f1 + i0 * i1,
    );
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
  | (
      (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as m,
      (`Real(_) | `Imag(_) | `Complex(_)) as v,
    ) =>
    MatrixUtil.map(m, divScalar(_, v))
  | _ => `NaN
  };

let equal = (a: value, b: value): bool =>
  switch (a, b) {
  | (
      (`Real(_) | `Imag(_) | `Complex(_)) as aV,
      (`Real(_) | `Imag(_) | `Complex(_)) as bV,
    ) =>
    equalScalar(aV, bV)
  | (`Vector2(a0, b0), `Vector2(a1, b1)) =>
    equalScalar(a0, a1) && equalScalar(b0, b1)
  | (`Vector3(a0, b0, c0), `Vector3(a1, b1, c1)) =>
    equalScalar(a0, a1) && equalScalar(b0, b1) && equalScalar(c0, c1)
  | (`Matrix2(a0, b0, c0, d0), `Matrix2(a1, b1, c1, d1)) =>
    equalScalar(a0, a1)
    && equalScalar(b0, b1)
    && equalScalar(c0, c1)
    && equalScalar(d0, d1)
  | (
      `Matrix3(a0, b0, c0, d0, e0, f0, g0, h0, i0),
      `Matrix3(a1, b1, c1, d1, e1, f1, g1, h1, i1),
    ) =>
    equalScalar(a0, a1)
    && equalScalar(b0, b1)
    && equalScalar(c0, c1)
    && equalScalar(d0, d1)
    && equalScalar(e0, e1)
    && equalScalar(f0, f1)
    && equalScalar(g0, g1)
    && equalScalar(h0, h1)
    && equalScalar(i0, i1)
  | _ => false
  };

let (~-) = neg;
let (+) = add;
let (-) = sub;
let ( * ) = mul;
let (/) = div;
let (==) = equal;
