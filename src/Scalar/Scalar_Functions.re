open Scalar_Types;
open Scalar_Base;

let neg = map(_, Real.neg);
let abs = map(_, Real.abs);
let round = map(_, Real.round);
let floor = map(_, Real.floor);
let ceil = map(_, Real.ceil);

let conj = (a: t): t =>
  switch (a) {
  | `Zero
  | `Real(_) => a
  | `Imag(im) => `Imag(Real.neg(im))
  | `Complex(re, im) => `Complex((re, Real.neg(im)))
  };

let%private map2Real = (a: t, b: t, fn: (Real.t, Real.t) => Real.t): t =>
  switch (a, b) {
  | (`Real(aRe), `Real(bRe)) => `Real(fn(aRe, bRe))
  | _ => nan
  };

let max = (a: t, b: t): t => map2Real(a, b, Real.max);
let min = (a: t, b: t): t => map2Real(a, b, Real.min);
let gcd = (a: t, b: t): t => map2Real(a, b, Real.gcd);
let lcm = (a: t, b: t): t => map2Real(a, b, Real.lcm);
