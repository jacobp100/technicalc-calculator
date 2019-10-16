open Types;

let arg = (re, im) =>
  if (Real.(re == zero)) {
    switch (Pervasives.compare(Real.toFloat(im), 0.0)) {
    | 1 => Real.Rational(1, 2, Pi)
    | (-1) => Rational(-1, 2, Pi)
    | _ => Real.nan
    };
  } else {
    let reF = Real.toFloat(re);
    let imF = Real.toFloat(im);
    Float(atan2(imF, reF));
  };

let expTuple = re =>
  switch (re) {
  | Real.Rational(0, 1, Unit) => Real.one
  | Rational(i, 1, Unit) => Rational(1, 0, Exp(i))
  | _ => Float(1.) // FIXME
  };

let rec exp = (a: value): value =>
  switch (a) {
  | `Zero => one
  | `Real(re) => `Real(expTuple(re))
  | `Imag(im) =>
    let re = Real_Trig.cos(im);
    let im = Real_Trig.sin(im);
    `Complex((re, im));
  | `Complex(re, im) =>
    let exp = expTuple(re);
    let re = Real_Trig.cos(im)->Real.mul(exp);
    let im = Real_Trig.sin(im)->Real.mul(exp);
    `Complex((re, im));
  | `Percent(p) => exp(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };

let logRealTuple = q =>
  switch (q) {
  | Real.Rational((-1), 1, Exp(reExp)) => Real.Rational(1, 1, Exp(reExp))
  // | (gtZero, reC) when Q.(gtZero > zero) => (
  //     QCUtil.mapFloat(gtZero, reC, Pervasives.log),
  //     Unit,
  //   )
  | _ => invalid_arg("logRealTuple")
  };

let rec log = (a: value): value =>
  switch (a) {
  | `Zero => `NaN
  // | `Real(gtZero) when Q.(gtZero > zero) =>
  //   let (reQ, reC) = logRealTuple(gtZero, reC);
  //   realQC(reQ, reC);
  | `Real(Rational((-1), 1, Unit)) => Base_Operators.mul(pi, i)
  | (`Real(_) | `Imag(_) | `Complex(_)) as vV =>
    let re =
      switch (vV) {
      | `Real(re) => Real.mul(re, re)
      | `Imag(im) => Real.mul(im, im)
      | `Complex(re, im) => Base_Operators.magnitudeSquared(re, im)
      };
    let re = logRealTuple(re);
    let re = Real.div(re, Rational(2, 1, Unit));
    let im =
      switch (vV) {
      | `Real(re) => arg(re, Rational(0, 1, Unit))
      | `Imag(im) => arg(Rational(0, 1, Unit), im)
      | `Complex(re, im) => arg(re, im)
      };
    complexQC(reQ, reC, imQ, imC);
  | `Percent(p) => log(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };