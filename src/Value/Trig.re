open Types;

let sin = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(reQ, reC) =>
    let (q, c) = TrigTuples.sinReal(reQ, reC);
    realQC(q, c);
  | `Imag(_)
  | `Complex(_) =>
    let iX = BasicMath.(x * i);
    BasicMath.(i * (Exp.exp(- iX) - Exp.exp(iX)) / ofInt(2));
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };

let sinh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(q, c) => QCUtil.toFloat(q, c)->sinh->ofFloat
  | `Imag(q, c) => BasicMath.(i * realQC(q, c)->sin)
  | `Complex(_) => BasicMath.((Exp.exp(x) - Exp.exp(- x)) / ofInt(2))
  | _ => `NaN
  };

let cos = (x: value): value =>
  switch (x) {
  | `Zero => one
  | `Real(reQ, reC) =>
    let (q, c) = TrigTuples.cosReal(reQ, reC);
    realQC(q, c);
  | `Imag(_)
  | `Complex(_) =>
    let iX = BasicMath.(x * i);
    BasicMath.((Exp.exp(iX) + Exp.exp(- iX)) / ofInt(2));
  | `Vector(_)
  | `Matrix(_)
  | `NaN => `NaN
  };

let cosh = (x: value): value =>
  switch (x) {
  | `Zero => one
  | `Real(q, c) => QCUtil.toFloat(q, c)->cosh->ofFloat
  | `Imag(q, c) => realQC(q, c)->cos
  | `Complex(_) => BasicMath.((Exp.exp(x) + Exp.exp(- x)) / ofInt(2))
  | _ => `NaN
  };

let tan = (x: value): value =>
  switch (TrigUtil.compareTrigReal(x)) {
  | `Zero
  | `Real(1 | 2, 1, `Pi) => zero
  | `Real(1 | 5, 4, `Pi) => one
  | `Real(3 | 7, 4, `Pi) => minusOne
  | `Real(1 | 4, 3, `Pi) => realQC(Q.of_int(1), Sqrt(Z.of_int(3)))
  | `Real(2 | 5, 3, `Pi) => realQC(Q.of_int(-1), Sqrt(Z.of_int(3)))
  | `Real(1 | 7, 6, `Pi) => realQC(Q.of_ints(1, 3), Sqrt(Z.of_int(3)))
  | `Real(5 | 11, 6, `Pi) => realQC(Q.of_ints(-1, 3), Sqrt(Z.of_int(3)))
  | `Real(1 | 3, 2, `Pi) => `NaN
  | `Real(_) => ValueUtil.mapRealFloat(x, tan)
  | `Imag(_)
  | `Complex(_) =>
    let iX = BasicMath.(x * i);
    let a = Exp.exp(iX);
    let b = Exp.exp(iX->BasicMath.neg);
    BasicMath.((a - b) / ((a + b) * i));
  | `Matrix
  | `Vector
  | `NaN => `NaN
  };

let tanh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(q, c) => QCUtil.toFloat(q, c)->tanh->ofFloat
  | `Imag(q, c) => BasicMath.(i * realQC(q, c)->tan)
  | `Complex(_) =>
    let a = Exp.exp(x);
    let b = Exp.exp(x->BasicMath.neg);
    BasicMath.((a - b) / (a + b));
  | _ => `NaN
  };

let asin = (a: value): value =>
  switch (TrigUtil.compareTrigReal(a)) {
  | `Real((-1), 1, `Unit) => realQC(Q.of_ints(-1, 2), Pi)
  | `Real((-1), 2, `Sqrt(3)) => realQC(Q.of_ints(-1, 3), Pi)
  | `Real((-1), 2, `Sqrt(2)) => realQC(Q.of_ints(-1, 4), Pi)
  | `Real((-1), 2, `Unit) => realQC(Q.of_ints(-1, 6), Pi)
  | `Zero => zero
  | `Real(1, 2, `Unit) => realQC(Q.of_ints(1, 6), Pi)
  | `Real(1, 2, `Sqrt(2)) => realQC(Q.of_ints(1, 4), Pi)
  | `Real(1, 2, `Sqrt(3)) => realQC(Q.of_ints(1, 3), Pi)
  | `Real(1, 1, `Unit) => realQC(Q.of_ints(1, 2), Pi)
  | `Real(_)
  | `Imag(_)
  | `Complex(_) =>
    switch (ValueUtil.realBounds(~lower=-1., ~upper=1., a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => ValueUtil.mapRealFloat(a, asin)
    | `Outside
    | `Imag(_)
    | `Complex(_) =>
      BasicMath.(Exp.(Pow.(- i * log(i * a + sqrt(one - a * a)))))
    | `NaN => `NaN
    }
  | `Matrix
  | `Vector
  | `NaN => `NaN
  };

let asinh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(q, c) => QCUtil.toFloat(q, c)->FloatUtil.asinh->ofFloat
  | `Imag(q, c) => BasicMath.(i * realQC(q, c)->asin)
  | `Complex(_) => BasicMath.(Exp.log(x + Pow.sqrt(x * x + one)))
  | _ => `NaN
  };

let acos = (a: value): value =>
  switch (TrigUtil.compareTrigReal(a)) {
  | `Real((-1), 1, `None) => realQC(Q.of_int(1), Pi)
  | `Real((-1), 2, `Sqrt(3)) => realQC(Q.of_ints(5, 6), Pi)
  | `Real((-1), 2, `Sqrt(2)) => realQC(Q.of_ints(3, 4), Pi)
  | `Real((-1), 2, `None) => realQC(Q.of_ints(2, 3), Pi)
  | `Zero => realQC(Q.of_ints(1, 2), Pi)
  | `Real(1, 2, `None) => realQC(Q.of_ints(1, 3), Pi)
  | `Real(1, 2, `Sqrt(2)) => realQC(Q.of_ints(1, 4), Pi)
  | `Real(1, 2, `Sqrt(3)) => realQC(Q.of_ints(1, 6), Pi)
  | `Real(1, 1, `None) => zero
  | `Real(_)
  | `Imag(_)
  | `Complex(_) =>
    switch (ValueUtil.realBounds(~lower=-1., ~upper=1., a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => ValueUtil.mapRealFloat(a, acos)
    | `Outside
    | `Imag(_)
    | `Complex(_) => BasicMath.(realQC(Q.of_ints(1, 2), Pi) - asin(a))
    | `NaN => `NaN
    }
  | `Matrix
  | `Vector
  | `NaN => `NaN
  };

let acosh = (x: value): value =>
  switch (ValueUtil.realBounds(~lower=1.0, x)) {
  | `Inside(f) => FloatUtil.acosh(f)->ofFloat
  | `LowerBound => zero
  | `BothBound
  | `UpperBound
  | `Outside
  /* acosh ix != i cosh x */
  | `Imag(_)
  | `Complex(_) =>
    /* From complex.js library */
    let res = acos(x);
    let (_, imf) = ValueUtil.toFloats(res);
    if (imf <= 0.) {
      BasicMath.(res * i);
    } else {
      BasicMath.(- res * i);
    };
  | `NaN => `NaN
  };

let atan = (a: value): value =>
  switch (TrigUtil.compareTrigReal(a)) {
  | `Real((-1), 1, `Sqrt(3)) => realQC(Q.of_ints(-1, 3), Pi)
  | `Real((-1), 1, `Unit) => realQC(Q.of_ints(-1, 4), Pi)
  | `Real((-1), 3, `Sqrt(3)) => realQC(Q.of_ints(-1, 6), Pi)
  | `Zero => zero
  | `Real(1, 3, `Sqrt(3)) => realQC(Q.of_ints(1, 6), Pi)
  | `Real(1, 1, `Unit) => realQC(Q.of_ints(1, 4), Pi)
  | `Real(1, 1, `Sqrt(3)) => realQC(Q.of_ints(1, 3), Pi)
  | `Real(_) => ValueUtil.mapRealFloat(a, atan)
  | `Imag(isAbsOne, Constant.Unit) when Q.(abs(isAbsOne) == one) => `NaN
  | (`Imag(_) | `Complex(_)) as vV =>
    let (reQ, reC, imQ, imC) =
      switch (vV) {
      | `Imag(imQ, imC) => (Q.zero, Constant.Unit, imQ, imC)
      | `Complex(reQ, reC, imQ, imC) => (reQ, reC, imQ, imC)
      };
    let a = QCUtil.toQ(reQ, reC);
    let b = QCUtil.toQ(imQ, imC);
    let b' = Q.(one - b);
    let d = Q.(a * a + b' * b');
    let two = Q.of_int(2);
    let (t1re, t1im) =
      complex(Q.((one - b * b - a * a) / d), Q.(- two * a / d))
      ->Exp.log
      ->ValueUtil.toQs;
    complex(Q.(- t1im / two), Q.(t1re / two));
  | `Matrix
  | `Vector
  | `NaN => `NaN
  };

let atanh = (x: value): value =>
  switch (ValueUtil.realBounds(~lower=-1.0, ~upper=1.0, x)) {
  | `Inside(f) => FloatUtil.atanh(f)->ofFloat
  | `BothBound
  | `LowerBound
  | `UpperBound => `NaN
  | `Imag(q, c) => BasicMath.(i * realQC(q, c)->atan)
  | `Outside
  | `Complex(_) =>
    let two = real(Q.of_int(2));
    BasicMath.(Exp.log((one + x) / (one - x)) / two);
  | `NaN => `NaN
  };
