open Types;

let re = (a: value): value =>
  switch (a) {
  | `Zero
  | `Imag(_) => `Zero
  | `Real(_) => a
  | `Complex(reQ, reC, _, _) => realQC(reQ, reC)
  | _ => `NaN
  };

let im = (a: value): value =>
  switch (a) {
  | `Zero
  | `Real(_) => `Zero
  | `Imag(_) => a
  | `Complex(_, _, imQ, imC) => imagQC(imQ, imC)
  | _ => `NaN
  };

let conjScalar = (a: scalar): scalar => {
  switch (a) {
  | `Zero
  | `Real(_) => a
  | `Imag(imQ, imC) => `Imag((Q.neg(imQ), imC))
  | `Complex(reQ, reC, imQ, imC) => `Complex((reQ, reC, Q.neg(imQ), imC))
  };
};

let conj = (a: value): value =>
  switch (a) {
  | `Zero => `Zero
  | `Real(_) => a
  | (`Imag(_) | `Complex(_)) as aS => conjScalar(aS)->valueOfScalar
  | (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as aM =>
    MatrixUtil.map(aM, conjScalar)
  | `NaN => `NaN
  };
