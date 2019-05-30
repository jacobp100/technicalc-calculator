open Types;

let mapQScalar = (a: scalar, f: Q.t => Q.t): scalar =>
  switch (a) {
  | `Zero => `Zero
  | `Real(aQ, aC) => `Real((f(aQ), aC))
  | `Imag(aQ, aC) => `Imag((f(aQ), aC))
  | `Complex(reQ, reC, imQ, imC) => `Complex((f(reQ), reC, f(imQ), imC))
  };

let negScalar = mapQScalar(_, Q.neg);

let absScalar = mapQScalar(_, Q.abs);

let floorScalar = mapQScalar(_, q => QUtil.floor(q)->Q.of_bigint);

let ceilScalar = mapQScalar(_, q => QUtil.ceil(q)->Q.of_bigint);

let roundScalar = mapQScalar(_, q => QUtil.round(q)->Q.of_bigint);

let mapQValue = (a: value, fn: scalar => scalar) =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV => fn(aV)->valueOfScalar
  | `Vector(elements) => `Vector(elements->Belt.Array.map(fn))
  | `Matrix(elements) => `Matrix(elements->Matrix.map(fn))
  | `NaN => `NaN
  };

let neg = mapQValue(_, negScalar);

let floor = mapQValue(_, floorScalar);

let ceil = mapQValue(_, ceilScalar);

let round = mapQValue(_, roundScalar);
