open Value_Core;

let%private mapReal = (x: t, f: Real.t => Real.t) =>
  switch (x) {
  | `Real(re) => `Real(f(re))
  | _ => x
  };

let%private realBounds = (~lower=?, ~upper=?, x: t) =>
  switch (x) {
  | `Zero => DecimalUtil.bounds(~lower?, ~upper?, Decimal.zero)
  | `Real(re) => DecimalUtil.bounds(~lower?, ~upper?, Real.toDecimal(re))
  | `Imag(im) => `Imag(im)
  | `Complex(re, im) => `Complex((re, im))
  | _ => `NaN
  };

let%private mapRealDecimal = (x: t, f: Decimal.t => Decimal.t): t =>
  switch (x) {
  | `Zero => ofDecimal(f(Decimal.zero))
  | `Real(re) => Real.toDecimal(re)->f->ofDecimal
  | _ => `NaN
  };

let sin = (x: t): t =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.sin(re)->ofReal
  | `Imag(_)
  | `Complex(_) =>
    let iX = x * i;
    i * (exp(- iX) - exp(iX)) / ofInt(2);
  | `Vector(_)
  | `Matrix(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let sinh = (x: t): t =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.toDecimal(re)->Decimal.sinh->ofDecimal
  | `Imag(im) => i * ofReal(im)->sin
  | `Complex(_) => (exp(x) - exp(- x)) / ofInt(2)
  | _ => `NaN
  };

let cos = (x: t): t =>
  switch (x) {
  | `Zero => one
  | `Real(re) => Real.cos(re)->ofReal
  | `Imag(_)
  | `Complex(_) =>
    let iX = x * i;
    (exp(iX) + exp(- iX)) / ofInt(2);
  | `Vector(_)
  | `Matrix(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let cosh = (x: t): t =>
  switch (x) {
  | `Zero => one
  | `Real(re) => Real.toDecimal(re)->Decimal.cosh->ofDecimal
  | `Imag(im) => ofReal(im)->cos
  | `Complex(_) => (exp(x) + exp(- x)) / ofInt(2)
  | _ => `NaN
  };

let tan = (x: t): t =>
  switch (mapReal(x, Real.mod2Pi)) {
  | `Zero
  | `Real(Rational(1 | 2, 1, Pi)) => zero
  | `Real(Rational(1 | 5, 4, Pi)) => one
  | `Real(Rational(3 | 7, 4, Pi)) => minusOne
  | `Real(Rational(1 | 4, 3, Pi)) => `Real(Real.rational(1, 1, Sqrt(3)))
  | `Real(Rational(2 | 5, 3, Pi)) => `Real(Real.rational(-1, 1, Sqrt(3)))
  | `Real(Rational(1 | 7, 6, Pi)) => `Real(Real.rational(1, 3, Sqrt(3)))
  | `Real(Rational(5 | 11, 6, Pi)) => `Real(Real.rational(-1, 3, Sqrt(3)))
  | `Real(Rational(1 | 3, 2, Pi)) => `NaN
  | `Real(_) => mapRealDecimal(x, Decimal.tan)
  | `Imag(_)
  | `Complex(_) =>
    let iX = x * i;
    let a = exp(iX);
    let b = exp(- iX);
    (a - b) / ((a + b) * i);
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let tanh = (x: t): t =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.toDecimal(re)->Decimal.tanh->ofDecimal
  | `Imag(im) => i * ofReal(im)->tan
  | `Complex(_) =>
    let a = exp(x);
    let b = exp(- x);
    (a - b) / (a + b);
  | _ => `NaN
  };

let asin = (a: t): t =>
  switch (mapReal(a, Real.mod2Pi)) {
  | `Real(Rational((-1), 1, Unit)) => `Real(Real.rational(-1, 2, Pi))
  | `Real(Rational((-1), 2, Sqrt(3))) => `Real(Real.rational(-1, 3, Pi))
  | `Real(Rational((-1), 2, Sqrt(2))) => `Real(Real.rational(-1, 4, Pi))
  | `Real(Rational((-1), 2, Unit)) => `Real(Real.rational(-1, 6, Pi))
  | `Zero => zero
  | `Real(Rational(1, 2, Unit)) => `Real(Real.rational(1, 6, Pi))
  | `Real(Rational(1, 2, Sqrt(2))) => `Real(Real.rational(1, 4, Pi))
  | `Real(Rational(1, 2, Sqrt(3))) => `Real(Real.rational(1, 3, Pi))
  | `Real(Rational(1, 1, Unit)) => `Real(Real.rational(1, 2, Pi))
  | `Real(_)
  | `Imag(_)
  | `Complex(_) =>
    switch (realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => mapRealDecimal(a, Decimal.asin)
    | `Outside
    | `Imag(_)
    | `Complex(_) => - i * log(i * a + sqrt(one - a * a))
    | `NaN => `NaN
    }
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let asinh = (x: t): t =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.toDecimal(re)->Decimal.asinh->ofDecimal
  | `Imag(im) => i * ofReal(im)->asin
  | `Complex(_) => log(x + sqrt(x * x + one))
  | _ => `NaN
  };

let acos = (a: t): t =>
  switch (mapReal(a, Real.mod2Pi)) {
  | `Real(Rational((-1), 1, Unit)) => `Real(Real.rational(1, 1, Pi))
  | `Real(Rational((-1), 2, Sqrt(3))) => `Real(Real.rational(5, 6, Pi))
  | `Real(Rational((-1), 2, Sqrt(2))) => `Real(Real.rational(3, 4, Pi))
  | `Real(Rational((-1), 2, Unit)) => `Real(Real.rational(2, 3, Pi))
  | `Zero => `Real(Real.rational(1, 2, Pi))
  | `Real(Rational(1, 2, Unit)) => `Real(Real.rational(1, 3, Pi))
  | `Real(Rational(1, 2, Sqrt(2))) => `Real(Real.rational(1, 4, Pi))
  | `Real(Rational(1, 2, Sqrt(3))) => `Real(Real.rational(1, 6, Pi))
  | `Real(Rational(1, 1, Unit)) => zero
  | `Real(_)
  | `Imag(_)
  | `Complex(_) =>
    switch (realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => mapRealDecimal(a, Decimal.acos)
    | `Outside
    | `Imag(_)
    | `Complex(_) => ofReal(Real.rational(1, 2, Pi)) - asin(a)
    | `NaN => `NaN
    }
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let acosh = (x: t): t =>
  switch (realBounds(~lower=Decimal.one, x)) {
  | `Inside(f) => Decimal.acosh(f)->ofDecimal
  | `LowerBound => zero
  | `BothBound
  | `UpperBound
  | `Outside
  /* acosh ix != i cosh x */
  | `Imag(_)
  | `Complex(_) =>
    /* From complex.js library */
    let res = acos(x);
    let imLteZero =
      switch (res) {
      | `Zero
      | `Real(_) => true
      | `Imag(im)
      | `Complex(_, im) => Real.(im <= zero)
      | _ => false
      };
    if (imLteZero) {
      res * i;
    } else {
      - res * i;
    };
  | `NaN => `NaN
  };

let atan = (a: t): t =>
  switch (mapReal(a, Real.mod2Pi)) {
  | `Real(Rational((-1), 1, Sqrt(3))) => `Real(Real.rational(-1, 3, Pi))
  | `Real(Rational((-1), 1, Unit)) => `Real(Real.rational(-1, 4, Pi))
  | `Real(Rational((-1), 3, Sqrt(3))) => `Real(Real.rational(-1, 6, Pi))
  | `Zero => zero
  | `Real(Rational(1, 3, Sqrt(3))) => `Real(Real.rational(1, 6, Pi))
  | `Real(Rational(1, 1, Unit)) => `Real(Real.rational(1, 4, Pi))
  | `Real(Rational(1, 1, Sqrt(3))) => `Real(Real.rational(1, 3, Pi))
  | `Real(_) => mapRealDecimal(a, Decimal.atan)
  | `Imag(Rational(1 | (-1), 1, Unit)) => `NaN
  | (`Imag(_) | `Complex(_)) as vV =>
    let (re, im) =
      switch (vV) {
      | `Imag(im) => (Real.zero, im)
      | `Complex(re, im) => (re, im)
      };
    let a = re;
    let b = im;
    let b' = Real.(one - b);
    let d = Real.(a * a + b' * b');
    let two = Real.ofInt(2);
    let t1 =
      ofComplex(Real.((one - b * b - a * a) / d), Real.(- two * a / d))->log;
    let (t1re, t1im) =
      switch (t1) {
      | `Zero => (Real.zero, Real.zero)
      | `Real(re) => (re, Real.zero)
      | `Imag(im) => (Real.zero, im)
      | `Complex(re, im) => (re, im)
      | _ => (Real.nan, Real.nan)
      };
    ofComplex(Real.(- t1im / two), Real.(t1re / two));
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let atanh = (x: t): t =>
  switch (realBounds(~lower=Decimal.minusOne, ~upper=Decimal.one, x)) {
  | `Inside(f) => Decimal.atanh(f)->ofDecimal
  | `BothBound
  | `LowerBound
  | `UpperBound => `NaN
  | `Imag(im) => i * ofReal(im)->atan
  | `Outside
  | `Complex(_) =>
    let two = ofInt(2);
    log((one + x) / (one - x)) / two;
  | `NaN => `NaN
  };
