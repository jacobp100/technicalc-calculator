open Types;

let rec re = (a: value): value =>
  switch (a) {
  | `Zero
  | `Imag(_) => `Zero
  | `Real(_) => a
  | `Complex(reQ, reC, _, _) => realQC(reQ, reC)
  | `Percent(p) => re(Base.percentToNumerical(p))
  | _ => `NaN
  };

let rec im = (a: value): value =>
  switch (a) {
  | `Zero
  | `Real(_) => `Zero
  | `Imag(_) => a
  | `Complex(_, _, imQ, imC) => imagQC(imQ, imC)
  | `Percent(p) => im(Base.percentToNumerical(p))
  | _ => `NaN
  };

let conjScalar = (a: scalar): scalar =>
  switch (a) {
  | `Zero
  | `Real(_) => a
  | `Imag(imQ, imC) => `Imag((Q.neg(imQ), imC))
  | `Complex(reQ, reC, imQ, imC) => `Complex((reQ, reC, Q.neg(imQ), imC))
  };

let conj = (a: value): value =>
  switch (a) {
  | `Zero => `Zero
  | `Real(_) => a
  | (`Imag(_) | `Complex(_)) as aS => conjScalar(aS)->valueOfScalar
  | `Percent(p) => `Percent(conjScalar(p))
  | `Vector(elements) => `Vector(elements->Belt.Array.map(conjScalar))
  | `Matrix(m) => `Matrix(m->Matrix.map(conjScalar))
  | `NaN => `NaN
  };
