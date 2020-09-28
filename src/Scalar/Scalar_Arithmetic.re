open Scalar_Types;

let add = (a: t, b: t): t =>
  switch (a, b) {
  | (`Zero, _) => b
  | (_, `Zero) => a
  | (`Real(a), `Real(b)) => `Real(Real.add(a, b))
  | (`Imag(a), `Imag(b)) => `Imag(Real.add(a, b))
  | (`Real(re), `Imag(im))
  | (`Imag(im), `Real(re)) => `Complex((re, im))
  | (`Real(aRe), `Complex(bRe, im))
  | (`Complex(bRe, im), `Real(aRe)) => `Complex((Real.add(aRe, bRe), im))
  | (`Imag(aIm), `Complex(re, bIm))
  | (`Complex(re, bIm), `Imag(aIm)) => `Complex((re, Real.add(aIm, bIm)))
  | (`Complex(aRe, aIm), `Complex(bRe, bIm)) =>
    `Complex((Real.add(aRe, bRe), Real.add(aIm, bIm)))
  };

let sub = (a: t, b: t): t => add(a, Scalar_Functions.neg(b));

let mul = (a: t, b: t): t =>
  switch (a, b) {
  | (`Zero, `Zero)
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_))
  | (`Real(_) | `Imag(_) | `Complex(_), `Zero) => `Zero
  | (`Real(a), `Real(b)) => `Real(Real.mul(a, b))
  | (`Imag(a), `Imag(b)) => `Real(Real.mul(a, b)->Real.neg)
  | (`Real(re), `Imag(im))
  | (`Imag(im), `Real(re)) => `Imag(Real.mul(re, im))
  | (`Real(aRe), `Complex(bRe, bIm))
  | (`Complex(bRe, bIm), `Real(aRe)) =>
    let re = Real.mul(aRe, bRe);
    let im = Real.mul(aRe, bIm);
    `Complex((re, im));
  | (`Imag(aIm), `Complex(bRe, bIm))
  | (`Complex(bRe, bIm), `Imag(aIm)) =>
    let re = Real.mul(aIm, bIm);
    let im = Real.mul(aIm, bRe);
    `Complex((Real.neg(re), im));
  | (`Complex(aRe, aIm), `Complex(bRe, bIm)) =>
    let reRe = Real.mul(aRe, bRe);
    let imIm = Real.mul(aIm, bIm);
    let reIm = Real.mul(aRe, bIm);
    let imRe = Real.mul(aIm, bRe);
    let reQ = Real.sub(reRe, imIm);
    let imQ = Real.add(reIm, imRe);
    `Complex((reQ, imQ));
  };

let%private magnitudeSquared = (re, im) => {
  let re = Real.mul(re, re);
  let im = Real.mul(im, im);
  Real.add(re, im);
};

let div = (a: t, b: t): t =>
  switch (a, b) {
  | (_, `Zero) => `Real(Real.nan)
  | (`Zero, _) => `Zero
  | (`Real(a), `Real(b)) => `Real(Real.div(a, b))
  | (`Imag(im), `Real(re)) => `Imag(Real.div(im, re))
  | (`Real(re), `Imag(im)) => `Imag(Real.div(Real.neg(re), im))
  | (`Imag(a), `Imag(b)) => `Real(Real.div(a, b))
  | (`Complex(aRe, aIm), `Real(bRe)) =>
    let re = Real.div(aRe, bRe);
    let im = Real.div(aIm, bRe);
    `Complex((re, im));
  | (`Complex(aRe, aIm), `Imag(bIm)) =>
    let re = Real.div(aIm, bIm);
    let im = Real.div(Real.neg(aRe), bIm);
    `Complex((re, im));
  | (`Real(aRe), `Complex(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let re = Real.mul(aRe, bRe)->Real.div(s);
    let im = Real.mul(Real.neg(aRe), bIm)->Real.div(s);
    `Complex((re, im));
  | (`Imag(aIm), `Complex(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let re = Real.mul(aIm, bIm)->Real.div(s);
    let im = Real.mul(aIm, bRe)->Real.div(s);
    `Complex((re, im));
  | (`Complex(_), `Complex(bRe, bIm)) =>
    let s = magnitudeSquared(bRe, bIm);
    let bRecipRe = Real.div(bRe, s);
    let bRecipIm = Real.div(Real.neg(bIm), s);
    let bRecip = `Complex((bRecipRe, bRecipIm));
    mul(a, bRecip);
  };
