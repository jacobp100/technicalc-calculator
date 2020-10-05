open Value_Types;
open Value_Base;

let arg = (re, im) =>
  if (Real.(re != zero)) {
    let reF = Real.toDecimal(re);
    let imF = Real.toDecimal(im);
    Real.Decimal(Decimal.atan2(imF, reF));
  } else if (Real.(im > zero)) {
    Real.ofRational(1, 2, Pi);
  } else if (Real.(im < zero)) {
    Real.ofRational(-1, 2, Pi);
  } else {
    Real.nan;
  };

let expReal = re =>
  switch (re) {
  | Real.Rational(0, 1, Unit) => Real.one
  | Rational(i, 1, Unit) => Real.ofRational(1, 1, Exp(i))
  | _ => Decimal(Real.toDecimal(re)->Decimal.exp)
  };

let rec exp = (a: t): t =>
  switch (a) {
  | `Zero => one
  | `Real(re) => ofReal(expReal(re))
  | `Imag(im) =>
    let re = Real_Trig.cos(im);
    let im = Real_Trig.sin(im);
    ofComplex(re, im);
  | `Complex(re, im) =>
    let exp = expReal(re);
    let re = Real_Trig.cos(im)->Real.mul(exp);
    let im = Real_Trig.sin(im)->Real.mul(exp);
    ofComplex(re, im);
  | `Percent(p) => exp(Value_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };

let logReal = q =>
  switch (q) {
  | Real.Rational(1, 1, Exp(reExp)) => Real.ofRational(reExp, 1, Unit)
  | _ =>
    let f = Real.toDecimal(q);
    if (Decimal.(f > zero)) {
      Decimal(Decimal.ln(f));
    } else {
      invalid_arg("logReal");
    };
  };

let rec log = (a: t): t =>
  switch (a) {
  | `Zero => `NaN
  | `Real(gtZero) when Real.(gtZero > zero) => ofReal(logReal(gtZero))
  | `Real(Rational((-1), 1, Unit)) => Value_Arithmetic.mul(pi, i)
  | (`Real(_) | `Imag(_) | `Complex(_)) as vV =>
    let re =
      switch (vV) {
      | `Real(re) => Real.mul(re, re)
      | `Imag(im) => Real.mul(im, im)
      | `Complex(re, im) => Real.add(Real.mul(re, re), Real.mul(im, im))
      };
    let re = Real.(div(logReal(re), ofRational(2, 1, Unit)));
    let im =
      switch (vV) {
      | `Real(re) => arg(re, Real.zero)
      | `Imag(im) => arg(Real.zero, im)
      | `Complex(re, im) => arg(re, im)
      };
    ofComplex(re, im);
  | `Percent(p) => log(Value_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };
