open Types;

let _mapReal = (x: value, f: Real.t => Real.t) =>
  switch (x) {
  | `Real(re) => `Real(f(re))
  | _ => x
  };

let _realBounds = (~lower=?, ~upper=?, x: value) =>
  switch (x) {
  | `Zero => FloatUtil.bounds(~lower?, ~upper?, 0.)
  | `Real(re) => FloatUtil.bounds(~lower?, ~upper?, Real.toFloat(re))
  | `Imag(im) => `Imag(im)
  | `Complex(re, im) => `Complex((re, im))
  | _ => `NaN
  };

let _mapRealFloat = (x: value, f: float => float): value =>
  switch (x) {
  | `Zero => ofFloat(f(0.))
  | `Real(re) => Real.toFloat(re)->f->ofFloat
  | _ => `NaN
  };

let sin = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.sin(re)->real
  | `Imag(_)
  | `Complex(_) =>
    let iX = Base.(x * i);
    Base.(i * (exp(- iX) - exp(iX)) / ofInt(2));
  | `Vector(_)
  | `Matrix(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let sinh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.toFloat(re)->sinh->ofFloat
  | `Imag(im) => Base.(i * real(im)->sin)
  | `Complex(_) => Base.((exp(x) - exp(- x)) / ofInt(2))
  | _ => `NaN
  };

let cos = (x: value): value =>
  switch (x) {
  | `Zero => one
  | `Real(re) => Real.cos(re)->real
  | `Imag(_)
  | `Complex(_) =>
    let iX = Base.(x * i);
    Base.((exp(iX) + exp(- iX)) / ofInt(2));
  | `Vector(_)
  | `Matrix(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let cosh = (x: value): value =>
  switch (x) {
  | `Zero => one
  | `Real(re) => Real.toFloat(re)->cosh->ofFloat
  | `Imag(im) => real(im)->cos
  | `Complex(_) => Base.((exp(x) + exp(- x)) / ofInt(2))
  | _ => `NaN
  };

let tan = (x: value): value =>
  switch (_mapReal(x, Real.mod2Pi)) {
  | `Zero
  | `Real(Rational(1 | 2, 1, Pi)) => zero
  | `Real(Rational(1 | 5, 4, Pi)) => one
  | `Real(Rational(3 | 7, 4, Pi)) => minusOne
  | `Real(Rational(1 | 4, 3, Pi)) => `Real(Rational(1, 1, Sqrt(3)))
  | `Real(Rational(2 | 5, 3, Pi)) => `Real(Rational(-1, 1, Sqrt(3)))
  | `Real(Rational(1 | 7, 6, Pi)) => `Real(Rational(1, 3, Sqrt(3)))
  | `Real(Rational(5 | 11, 6, Pi)) => `Real(Rational(-1, 3, Sqrt(3)))
  | `Real(Rational(1 | 3, 2, Pi)) => `NaN
  | `Real(_) => _mapRealFloat(x, tan)
  | `Imag(_)
  | `Complex(_) =>
    let iX = Base.(x * i);
    let a = Base.(exp(iX));
    let b = Base.(exp(- iX));
    Base.((a - b) / ((a + b) * i));
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let tanh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.toFloat(re)->tanh->ofFloat
  | `Imag(im) => Base.(i * real(im)->tan)
  | `Complex(_) =>
    let a = Base.(exp(x));
    let b = Base.(exp(- x));
    Base.((a - b) / (a + b));
  | _ => `NaN
  };

let asin = (a: value): value =>
  switch (_mapReal(a, Real.mod2Pi)) {
  | `Real(Rational((-1), 1, Unit)) => `Real(Rational(-1, 2, Pi))
  | `Real(Rational((-1), 2, Sqrt(3))) => `Real(Rational(-1, 3, Pi))
  | `Real(Rational((-1), 2, Sqrt(2))) => `Real(Rational(-1, 4, Pi))
  | `Real(Rational((-1), 2, Unit)) => `Real(Rational(-1, 6, Pi))
  | `Zero => zero
  | `Real(Rational(1, 2, Unit)) => `Real(Rational(1, 6, Pi))
  | `Real(Rational(1, 2, Sqrt(2))) => `Real(Rational(1, 4, Pi))
  | `Real(Rational(1, 2, Sqrt(3))) => `Real(Rational(1, 3, Pi))
  | `Real(Rational(1, 1, Unit)) => `Real(Rational(1, 2, Pi))
  | `Real(_)
  | `Imag(_)
  | `Complex(_) =>
    switch (_realBounds(~lower=-1., ~upper=1., a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => _mapRealFloat(a, asin)
    | `Outside
    | `Imag(_)
    | `Complex(_) => Base.(- i * log(i * a + sqrt(one - a * a)))
    | `NaN => `NaN
    }
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let asinh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(re) => Real.toFloat(re)->FloatUtil.asinh->ofFloat
  | `Imag(im) => Base.(i * real(im)->asin)
  | `Complex(_) => Base.(log(x + sqrt(x * x + one)))
  | _ => `NaN
  };

let acos = (a: value): value =>
  switch (_mapReal(a, Real.mod2Pi)) {
  | `Real(Rational((-1), 1, Unit)) => `Real(Rational(1, 1, Pi))
  | `Real(Rational((-1), 2, Sqrt(3))) => `Real(Rational(5, 6, Pi))
  | `Real(Rational((-1), 2, Sqrt(2))) => `Real(Rational(3, 4, Pi))
  | `Real(Rational((-1), 2, Unit)) => `Real(Rational(2, 3, Pi))
  | `Zero => `Real(Rational(1, 2, Pi))
  | `Real(Rational(1, 2, Unit)) => `Real(Rational(1, 3, Pi))
  | `Real(Rational(1, 2, Sqrt(2))) => `Real(Rational(1, 4, Pi))
  | `Real(Rational(1, 2, Sqrt(3))) => `Real(Rational(1, 6, Pi))
  | `Real(Rational(1, 1, Unit)) => zero
  | `Real(_)
  | `Imag(_)
  | `Complex(_) =>
    switch (_realBounds(~lower=-1., ~upper=1., a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => _mapRealFloat(a, acos)
    | `Outside
    | `Imag(_)
    | `Complex(_) => Base.(`Real(Rational(1, 2, Pi)) - asin(a))
    | `NaN => `NaN
    }
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let acosh = (x: value): value =>
  switch (_realBounds(~lower=1.0, x)) {
  | `Inside(f) => FloatUtil.acosh(f)->ofFloat
  | `LowerBound => zero
  | `BothBound
  | `UpperBound
  | `Outside
  /* acosh ix != i cosh x */
  | `Imag(_)
  | `Complex(_) =>
    /* From complex.js library */
    let res = acos(x);
    let im =
      switch (x) {
      | `Zero
      | `Real(_) => Real.zero
      | `Imag(im)
      | `Complex(_, im) => im
      | _ => Real.nan
      };
    if (Real.toFloat(im) <= 0.) {
      Base.(res * i);
    } else {
      Base.(- res * i);
    };
  | `NaN => `NaN
  };

let atan = (a: value): value =>
  switch (_mapReal(a, Real.mod2Pi)) {
  | `Real(Rational((-1), 1, Sqrt(3))) => `Real(Rational(-1, 3, Pi))
  | `Real(Rational((-1), 1, Unit)) => `Real(Rational(-1, 4, Pi))
  | `Real(Rational((-1), 3, Sqrt(3))) => `Real(Rational(-1, 6, Pi))
  | `Zero => zero
  | `Real(Rational(1, 3, Sqrt(3))) => `Real(Rational(1, 6, Pi))
  | `Real(Rational(1, 1, Unit)) => `Real(Rational(1, 4, Pi))
  | `Real(Rational(1, 1, Sqrt(3))) => `Real(Rational(1, 3, Pi))
  | `Real(_) => _mapRealFloat(a, atan)
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
    let two = Real.fromInt(2);
    let t1 =
      complex(Real.((one - b * b - a * a) / d), Real.(- two * a / d))
      ->Base.log;
    let (t1re, t1im) =
      switch (t1) {
      | `Zero => (Real.zero, Real.zero)
      | `Real(re) => (re, Real.zero)
      | `Imag(im) => (Real.zero, im)
      | `Complex(re, im) => (re, im)
      | _ => (Real.nan, Real.nan)
      };
    complex(Real.(- t1im / two), Real.(t1re / two));
  | `Matrix(_)
  | `Vector(_)
  | `Percent(_)
  | `NaN => `NaN
  };

let atanh = (x: value): value =>
  switch (_realBounds(~lower=-1.0, ~upper=1.0, x)) {
  | `Inside(f) => FloatUtil.atanh(f)->ofFloat
  | `BothBound
  | `LowerBound
  | `UpperBound => `NaN
  | `Imag(im) => Base.(i * real(im)->atan)
  | `Outside
  | `Complex(_) =>
    let two = real(Rational(2, 1, Unit));
    Base.(log((one + x) / (one - x)) / two);
  | `NaN => `NaN
  };