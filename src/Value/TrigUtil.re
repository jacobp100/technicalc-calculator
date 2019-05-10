let trigPeriod = (q, c) =>
  c == Constant.Pi ? (QUtil.safeMod(q, Z.of_int(2)), c) : (q, c);

let compareTrig = (q, c) => {
  let (q, c) = trigPeriod(q, c);
  ValueUtil.compareTuple(q, c);
};

let compareTrigReal = a =>
  switch (a) {
  | `Zero => `Zero
  | `Real(reQ, reC) => `Real(compareTrig(reQ, reC))
  | `Imag(imQ, imC) => `Imag((imQ, imC))
  | `Complex(reQ, reC, imQ, imC) => `Complex((reQ, reC, imQ, imC))
  | `Vector(_) => `Vector
  | `Matrix(_) => `Matrix
  | `NaN => `NaN
  };
