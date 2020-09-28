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
