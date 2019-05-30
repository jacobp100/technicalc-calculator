open Types;

let toFloats = (x: value): (float, float) =>
  switch (x) {
  | `Zero => (0., 0.)
  | `Real(reQ, reC) => (QCUtil.toFloat(reQ, reC), 0.)
  | `Imag(imQ, imC) => (0., QCUtil.toFloat(imQ, imC))
  | `Complex(reQ, reC, imQ, imC) => (
      QCUtil.toFloat(reQ, reC),
      QCUtil.toFloat(imQ, imC),
    )
  | _ => Pervasives.(nan, nan)
  };

let toQs = (x: value): (Q.t, Q.t) =>
  switch (x) {
  | `Zero => (Q.zero, Q.zero)
  | `Real(reQ, reC) => (QCUtil.toQ(reQ, reC), Q.zero)
  | `Imag(imQ, imC) => (Q.zero, QCUtil.toQ(imQ, imC))
  | `Complex(reQ, reC, imQ, imC) => (
      QCUtil.toQ(reQ, reC),
      QCUtil.toQ(imQ, imC),
    )
  | _ => (Q.undef, Q.undef)
  };

let realBounds = (~lower=?, ~upper=?, x: value) =>
  switch (x) {
  | `Zero => FloatUtil.bounds(~lower?, ~upper?, 0.)
  | `Real(q, c) => FloatUtil.bounds(~lower?, ~upper?, QCUtil.toFloat(q, c))
  | `Imag(imQ, imC) => `Imag((imQ, imC))
  | `Complex(reQ, reC, imQ, imC) => `Complex((reQ, reC, imQ, imC))
  | _ => `NaN
  };

let mapRealFloat = (x: value, f: float => float): value =>
  switch (x) {
  | `Zero => `Real((Q.of_float(f(0.)), Constant.Unit))
  | `Real(q, c) => `Real((QCUtil.mapFloat(q, c, f), Constant.Unit))
  | _ => `NaN
  };

let compareConstant = c =>
  switch (c) {
  | Constant.Unit => `Unit
  | Pi => `Pi
  | Sqrt(cc) => `Sqrt(Z.to_int(cc))
  | Exp(cc) => `Exp(cc)
  };

let compareTuple = (q, c) =>
  switch (Q.num(q)->Z.to_int, Q.den(q)->Z.to_int, compareConstant(c)) {
  | (n, d, c) => (n, d, c)
  | exception Z.Overflow => (0, 0, `Unit)
  };
