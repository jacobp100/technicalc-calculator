open Types;

let g = 4.7421875;

let p0 = 0.99999999999999709182;

let p = [|
  57.156235665862923517,
  (-59.597960355475491248),
  14.136097974741747174,
  (-0.49191381609762019978),
  0.33994649984811888699e-4,
  0.46523628927048575665e-4,
  (-0.98374475304879564677e-4),
  0.15808870322491248884e-3,
  (-0.21026444172410488319e-3),
  0.21743961811521264320e-3,
  (-0.16431810653676389022e-3),
  0.84418223983852743293e-4,
  (-0.26190838401581408670e-4),
  0.36899182659531622704e-5,
|];

let sqrt2Pi = 2.506628274631000502415765284811;

let gamma = (x: value): value =>
  switch (x) {
  | `Zero => `NaN
  | `Real(Rational(1 | 2, 1, Unit)) => real(Real.fromInt(1))
  | `Real(Rational(3, 1, Unit)) => real(Real.fromInt(2))
  | `Real(Rational(4, 1, Unit)) => real(Real.fromInt(6))
  | `Real(Rational(5, 1, Unit)) => real(Real.fromInt(24))
  | `Real(Rational(6, 1, Unit)) => real(Real.fromInt(120))
  | `Real(Rational(7, 1, Unit)) => real(Real.fromInt(720))
  | `Real(Rational(8, 1, Unit)) => real(Real.fromInt(5040))
  | `Real(Rational(9, 1, Unit)) => real(Real.fromInt(40320))
  | `Real(Rational(10, 1, Unit)) => real(Real.fromInt(362880))
  | `Real(Rational(11, 1, Unit)) => real(Real.fromInt(3628800))
  | `Real(Rational(12, 1, Unit)) => real(Real.fromInt(479001600))
  | `Real(re) when Real.(re > zero) =>
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let n = Real.toFloat(re) -. 1.;
    let x =
      p->Belt.Array.reduceWithIndex(
        p0,
        (accum, pi, i) => {
          let i = i + 1;
          accum +. pi /. (n +. float_of_int(i));
        },
      );
    let t = n +. g +. 0.5;
    let n = n +. 0.5;
    let x = x;
    ofFloat(sqrt2Pi *. t ** n *. exp(-. t) *. x);
  | (`Imag(_) | `Complex(_)) as xV =>
    let (nRe, nIm) =
      switch (xV) {
      | `Imag(im) => (0., Real.toFloat(im))
      | `Complex(re, im) => (Real.toFloat(re), Real.toFloat(im))
      };
    let (xRe, xIm) =
      p->Belt.Array.reduceWithIndex(
        (p0, 0.),
        ((re, im), p, i) => {
          let i = i + 1;
          let real = nRe +. float_of_int(i);
          let deno = real *. real +. nIm *. nIm;
          if (deno != 0.) {
            (re +. p *. real /. deno, im +. -. (p *. nIm) /. deno);
          } else if (p < 0.) {
            (infinity, im);
          } else {
            (-. infinity, im);
          };
        },
      );

    let nRe = nRe +. 0.5;
    let t = complex(Real.Float(nRe +. g), Real.Float(nIm));

    let result =
      if (FloatUtil.isFinite(nRe)) {
        let n = complex(Real.Float(nRe), Real.Float(nIm));
        Base.(t ** n * real(Real.Float(sqrt2Pi)));
      } else {
        zero;
      };

    let t = Base.(exp(- t));
    let x = complex(Real.Float(xRe), Real.Float(xIm));

    Base.(result * t * x);
  | _ => `NaN
  };

let factorial = x => Base.(x + one)->gamma;