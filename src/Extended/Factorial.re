open Types;

let g = Decimal.ofFloat(4.7421875);

let p0 = Decimal.ofFloat(0.99999999999999709182);

let p = [|
  Decimal.ofFloat(57.156235665862923517),
  Decimal.ofFloat(-59.597960355475491248),
  Decimal.ofFloat(14.136097974741747174),
  Decimal.ofFloat(-0.49191381609762019978),
  Decimal.ofFloat(0.33994649984811888699e-4),
  Decimal.ofFloat(0.46523628927048575665e-4),
  Decimal.ofFloat(-0.98374475304879564677e-4),
  Decimal.ofFloat(0.15808870322491248884e-3),
  Decimal.ofFloat(-0.21026444172410488319e-3),
  Decimal.ofFloat(0.21743961811521264320e-3),
  Decimal.ofFloat(-0.16431810653676389022e-3),
  Decimal.ofFloat(0.84418223983852743293e-4),
  Decimal.ofFloat(-0.26190838401581408670e-4),
  Decimal.ofFloat(0.36899182659531622704e-5),
|];

let half = Decimal.ofFloat(0.5);
let sqrt2Pi = Decimal.ofFloat(2.506628274631000502415765284811);

let gamma = (x: value): value =>
  switch (x) {
  | `Zero => `NaN
  | `Real(Rational(1 | 2, 1, Unit)) => real(Real.ofInt(1))
  | `Real(Rational(3, 1, Unit)) => real(Real.ofInt(2))
  | `Real(Rational(4, 1, Unit)) => real(Real.ofInt(6))
  | `Real(Rational(5, 1, Unit)) => real(Real.ofInt(24))
  | `Real(Rational(6, 1, Unit)) => real(Real.ofInt(120))
  | `Real(Rational(7, 1, Unit)) => real(Real.ofInt(720))
  | `Real(Rational(8, 1, Unit)) => real(Real.ofInt(5040))
  | `Real(Rational(9, 1, Unit)) => real(Real.ofInt(40320))
  | `Real(Rational(10, 1, Unit)) => real(Real.ofInt(362880))
  | `Real(Rational(11, 1, Unit)) => real(Real.ofInt(3628800))
  | `Real(Rational(12, 1, Unit)) => real(Real.ofInt(479001600))
  | `Real(re) when Real.(re > zero) =>
    open Decimal;
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let n = Real.toDecimal(re) - half;
    let x =
      p->Belt.Array.reduceWithIndex(p0, (accum, pi, i) =>
        accum + pi / (n + ofInt(i) + half)
      );
    let t = n + g;
    ofDecimal(sqrt2Pi * t ** n * exp(- t) * x);
  | (`Imag(_) | `Complex(_)) as xV =>
    open Decimal;
    let (nRe, nIm) =
      switch (xV) {
      | `Imag(im) => (zero, Real.toDecimal(im))
      | `Complex(re, im) => (Real.toDecimal(re), Real.toDecimal(im))
      };
    let nRe = nRe - half;
    let n = complex(Real.Decimal(nRe), Real.Decimal(nIm));
    let (xRe, xIm) =
      p->Belt.Array.reduceWithIndex(
        (p0, zero),
        ((re, im), p, i) => {
          let real = nRe + ofInt(i) + half;
          let den = real * real + nIm * nIm;
          if (den != zero) {
            (re + p * real / den, im - p * nIm / den);
          } else {
            (nan, im);
          };
        },
      );
    let x = complex(Real.Decimal(xRe), Real.Decimal(xIm));
    let t = Base.(n + ofDecimal(g));
    Base.(ofDecimal(sqrt2Pi) * t ** n * exp(- t) * x);
  | _ => `NaN
  };

let factorial = x => Base.(x + one)->gamma;