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

let expReal = re =>
  switch (re) {
  | Real.Rational(0, 1, Unit) => Real.one
  | Rational(i, 1, Unit) => Rational(1, 0, Exp(i))
  | _ => Float(Real.toFloat(re)->exp)
  };

let rec exp = (a: value): value =>
  switch (a) {
  | `Zero => one
  | `Real(re) => `Real(expReal(re))
  | `Imag(im) =>
    let re = Real_Trig.cos(im);
    let im = Real_Trig.sin(im);
    `Complex((re, im));
  | `Complex(re, im) =>
    let exp = expReal(re);
    let re = Real_Trig.cos(im)->Real.mul(exp);
    let im = Real_Trig.sin(im)->Real.mul(exp);
    `Complex((re, im));
  | `Percent(p) => exp(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };

let logReal = q =>
  switch (q) {
  | Real.Rational((-1), 1, Exp(reExp)) => Real.Rational(1, 1, Exp(reExp))
  | _ =>
    let f = Real.toFloat(q);
    if (f > 0.) {
      Float(Pervasives.log(f));
    } else {
      invalid_arg("logReal");
    };
  };

let rec log = (a: value): value =>
  switch (a) {
  | `Zero => `NaN
  | `Real(gtZero) when Real.toFloat(gtZero) > 0. => `Real(logReal(gtZero))
  | `Real(Rational((-1), 1, Unit)) => Base_Operators.mul(pi, i)
  | (`Real(_) | `Imag(_) | `Complex(_)) as vV =>
    let re =
      switch (vV) {
      | `Real(re) => Real.mul(re, re)
      | `Imag(im) => Real.mul(im, im)
      | `Complex(re, im) => Base_Operators.magnitudeSquared(re, im)
      };
    let re = logReal(re)->Real.div(Rational(2, 1, Unit));
    let im =
      switch (vV) {
      | `Real(re) => arg(re, Real.zero)
      | `Imag(im) => arg(Real.zero, im)
      | `Complex(re, im) => arg(re, im)
      };
    `Complex((re, im));
  | `Percent(p) => log(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };