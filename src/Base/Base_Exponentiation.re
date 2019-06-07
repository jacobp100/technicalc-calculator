open Types;

let argTuple = (reQ, reC, imQ, imC) =>
  if (Q.(reQ == zero)) {
    switch (Pervasives.compare(QCUtil.toFloat(imQ, imC), 0.0)) {
    | 1 => (Q.of_ints(1, 2), Constant.Pi)
    | (-1) => (Q.of_ints(-1, 2), Pi)
    | _ => (Q.undef, Unit)
    };
  } else {
    let reF = QCUtil.toFloat(reQ, reC);
    let imF = QCUtil.toFloat(imQ, imC);
    let q = atan2(imF, reF)->Q.of_float;
    (q, Unit);
  };

let expTuple = (q, c) =>
  switch (q, c) {
  | (isZero, Constant.Unit) when Q.(isZero == zero) => (Q.one, Constant.Unit)
  | (isInt, Unit) when QUtil.isInt(isInt) =>
    switch (Q.num(q)->Z.to_int) {
    | i => (Q.one, Exp(i))
    | exception Z.Overflow => (Q.undef, Unit)
    }
  | _ => (QCUtil.mapFloat(q, c, exp), Unit)
  };

let rec exp = (a: value): value =>
  switch (a) {
  | `Zero => one
  | `Real(reQ, reC) =>
    let (q, c) = expTuple(reQ, reC);
    realQC(q, c);
  | `Imag(imQ, imC) =>
    let (reQ, reC) = TrigTuples.cosReal(imQ, imC);
    let (imQ, imC) = TrigTuples.sinReal(imQ, imC);
    complexQC(reQ, reC, imQ, imC);
  | `Complex(aReQ, aReC, aImQ, aImC) =>
    let (expQ, expC) = expTuple(aReQ, aReC);
    let (reQ, reC) = TrigTuples.cosReal(aImQ, aImC);
    let (imQ, imC) = TrigTuples.sinReal(aImQ, aImC);
    let (reQ, reC) = Base_Operators.mulTuple(expQ, expC, reQ, reC);
    let (imQ, imC) = Base_Operators.mulTuple(expQ, expC, imQ, imC);
    complexQC(reQ, reC, imQ, imC);
  | `Percent(p) => exp(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };

let logRealTuple = (q, c) =>
  switch (q, c) {
  | (isMinusOne, Constant.Exp(reExp)) when Q.(isMinusOne == minus_one) => (
      Q.one,
      Constant.Exp(reExp),
    )
  | (gtZero, reC) when Q.(gtZero > zero) => (
      QCUtil.mapFloat(gtZero, reC, Pervasives.log),
      Unit,
    )
  | _ => invalid_arg("logRealTuple")
  };

let rec log = (a: value): value =>
  switch (a) {
  | `Zero => `NaN
  | `Real(gtZero, reC) when Q.(gtZero > zero) =>
    let (reQ, reC) = logRealTuple(gtZero, reC);
    realQC(reQ, reC);
  | `Real(isMinusOne, Constant.Unit) when Q.(isMinusOne == minus_one) =>
    Base_Operators.mul(pi, i)
  | (`Real(_) | `Imag(_) | `Complex(_)) as vV =>
    let (reQ, reC) =
      switch (vV) {
      | `Real(reQ, reC) => Base_Operators.mulTuple(reQ, reC, reQ, reC)
      | `Imag(imQ, imC) => Base_Operators.mulTuple(imQ, imC, imQ, imC)
      | `Complex(reQ, reC, imQ, imC) =>
        Base_Operators.magnitudeSquaredTuple(reQ, reC, imQ, imC)
      };
    let (reQ, reC) = logRealTuple(reQ, reC);
    let (reQ, reC) = Base_Operators.divTuple(reQ, reC, Q.of_int(2), Unit);
    let (imQ, imC) =
      switch (vV) {
      | `Real(reQ, reC) => argTuple(reQ, reC, Q.zero, Unit)
      | `Imag(imQ, imC) => argTuple(Q.zero, Unit, imQ, imC)
      | `Complex(reQ, reC, imQ, imC) => argTuple(reQ, reC, imQ, imC)
      };
    complexQC(reQ, reC, imQ, imC);
  | `Percent(p) => log(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };
