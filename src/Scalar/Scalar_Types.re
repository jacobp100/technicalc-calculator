type t = [
  | `Zero
  | `Real(Real.t)
  | `Imag(Real.t)
  | `Complex(Real.t, Real.t)
];
