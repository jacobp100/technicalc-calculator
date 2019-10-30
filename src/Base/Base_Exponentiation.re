open Types;

let arg = (re, im) =>
  if (Real.(re != zero)) {
    let reF = Real.toDecimal(re);
    let imF = Real.toDecimal(im);
    Real.Decimal(Decimal.atan2(imF, reF));
  } else if (Real.(im > zero)) {
    Real.rational(1, 2, Pi);
  } else if (Real.(im < zero)) {
    Real.rational(-1, 2, Pi);
  } else {
    Real.nan;
  };

let expReal = re =>
  switch (re) {
  | Real.Rational(0, 1, Unit) => Real.one
  | Rational(i, 1, Unit) => Real.rational(1, 1, Exp(i))
  | _ => Decimal(Real.toDecimal(re)->Decimal.exp)
  };

let rec exp = (a: value): value =>
  switch (a) {
  | `Zero => one
  | `Real(re) => real(expReal(re))
  | `Imag(im) =>
    let re = Real_Trig.cos(im);
    let im = Real_Trig.sin(im);
    complex(re, im);
  | `Complex(re, im) =>
    let exp = expReal(re);
    let re = Real_Trig.cos(im)->Real.mul(exp);
    let im = Real_Trig.sin(im)->Real.mul(exp);
    complex(re, im);
  | `Percent(p) => exp(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };

let logReal = q =>
  switch (q) {
  | Real.Rational((-1), 1, Exp(reExp)) => Real.rational(1, 1, Exp(reExp))
  | _ =>
    let f = Real.toDecimal(q);
    if (Decimal.(f > zero)) {
      Decimal(Decimal.ln(f));
    } else {
      invalid_arg("logReal");
    };
  };

let rec log = (a: value): value =>
  switch (a) {
  | `Zero => `NaN
  | `Real(gtZero) when Real.(gtZero > zero) => real(logReal(gtZero))
  | `Real(Rational((-1), 1, Unit)) => Base_Operators.mul(pi, i)
  | (`Real(_) | `Imag(_) | `Complex(_)) as vV =>
    let re =
      switch (vV) {
      | `Real(re) => Real.mul(re, re)
      | `Imag(im) => Real.mul(im, im)
      | `Complex(re, im) => Base_Operators.magnitudeSquared(re, im)
      };
    let re = logReal(re)->Real.(div(rational(2, 1, Unit)));
    let im =
      switch (vV) {
      | `Real(re) => arg(re, Real.zero)
      | `Imag(im) => arg(Real.zero, im)
      | `Complex(re, im) => arg(re, im)
      };
    complex(re, im);
  | `Percent(p) => log(Base_Util.percentToNumerical(p))
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };