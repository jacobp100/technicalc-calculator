open Types;

let sin = (a: value): value =>
  switch (a) {
  | `Zero => zero
  | `Real(reQ, reC) =>
    let (q, c) = TrigTuples.sinReal(reQ, reC);
    `Real((q, c));
  | `Imag(_)
  | `Complex(_) =>
    let iA = BasicMath.(a * i);
    BasicMath.(
      (Exp.exp(- iA) - Exp.exp(iA)) * `Imag((Q.of_ints(1, 2), Unit))
    );
  | `Vector2(_)
  | `Vector3(_)
  | `Matrix2(_)
  | `Matrix3(_)
  | `NaN => `NaN
  };

let cos = (a: value): value =>
  switch (a) {
  | `Zero => one
  | `Real(reQ, reC) =>
    let (q, c) = TrigTuples.cosReal(reQ, reC);
    `Real((q, c));
  | `Imag(_)
  | `Complex(_) =>
    let iA = BasicMath.(a * i);
    BasicMath.(
      (Exp.exp(iA) + Exp.exp(- iA)) * `Real((Q.of_ints(1, 2), Unit))
    );
  | `Vector2(_)
  | `Vector3(_)
  | `Matrix2(_)
  | `Matrix3(_)
  | `NaN => `NaN
  };

let tan = (a: value): value =>
  switch (TrigUtil.compareTrigReal(a)) {
  | `Zero
  | `Real(0 | 1 | 2, 1, `Pi) => zero
  | `Real(1 | 5, 4, `Pi) => one
  | `Real(3 | 7, 4, `Pi) => minusOne
  | `Real(1 | 4, 3, `Pi) => realQC(Q.of_int(1), Sqrt(Z.of_int(3)))
  | `Real(2 | 5, 3, `Pi) => realQC(Q.of_int(-1), Sqrt(Z.of_int(3)))
  | `Real(1 | 7, 6, `Pi) => realQC(Q.of_ints(1, 3), Sqrt(Z.of_int(3)))
  | `Real(5 | 11, 6, `Pi) => realQC(Q.of_ints(-1, 3), Sqrt(Z.of_int(3)))
  | `Real(1 | 3, 2, `Pi) => `NaN
  | `Real(_) => ValueUtil.mapRealFloat(a, tan)
  | `Complex(_) =>
    let iA = BasicMath.(a * i);
    let x = Exp.exp(iA);
    let y = Exp.exp(iA->BasicMath.neg);
    BasicMath.((x - y) / ((x + y) * i));
  | `Matrix
  | `NaN => `NaN
  };

let asin = (a: value): value =>
  switch (TrigUtil.compareTrigReal(a)) {
  | `Real((-1), 1, `Unit) => realQC(Q.of_ints(-1, 2), Pi)
  | `Real((-1), 2, `Sqrt(3)) => realQC(Q.of_ints(-1, 3), Pi)
  | `Real((-1), 2, `Sqrt(2)) => realQC(Q.of_ints(-1, 4), Pi)
  | `Real((-1), 2, `Unit) => realQC(Q.of_ints(-1, 6), Pi)
  | `Zero
  | `Real(0, 1, `Unit) => `Zero
  | `Real(1, 2, `Unit) => realQC(Q.of_ints(1, 6), Pi)
  | `Real(1, 2, `Sqrt(2)) => realQC(Q.of_ints(1, 4), Pi)
  | `Real(1, 2, `Sqrt(3)) => realQC(Q.of_ints(1, 3), Pi)
  | `Real(1, 1, `Unit) => realQC(Q.of_ints(1, 2), Pi)
  | `Real(_)
  | `Complex(_) =>
    switch (ValueUtil.realBounds(~lower=-1., ~upper=1., a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => ValueUtil.mapRealFloat(a, asin)
    | `Outside
    | `Complex(_) => BasicMath.(- i * Exp.log(i * a + Pow.sqrt(one - a * a)))
    | `NaN => `NaN
    }
  | `Matrix
  | `NaN => `NaN
  };

let acos = (a: value): value =>
  switch (TrigUtil.compareTrigReal(a)) {
  | `Zero
  | `Real(0, 1, `Unit)
  | `Real(1 | 2, 1, `Pi) => zero
  | `Real(1, 2, `Pi) => one
  | `Real(3, 2, `Pi) => minusOne
  | `Real(1 | 2, 3, `Pi) => realQC(Q.of_ints(1, 2), Sqrt(Z.of_int(3)))
  | `Real(4 | 5, 3, `Pi) => realQC(Q.of_ints(-1, 2), Sqrt(Z.of_int(3)))
  | `Real(1 | 3, 4, `Pi) => realQC(Q.of_ints(1, 2), Sqrt(Z.of_int(2)))
  | `Real(5 | 7, 4, `Pi) => realQC(Q.of_ints(-1, 2), Sqrt(Z.of_int(2)))
  | `Real(1 | 5, 6, `Pi) => realQC(Q.of_ints(1, 2), Unit)
  | `Real(7 | 11, 6, `Pi) => realQC(Q.of_ints(-1, 2), Unit)
  | `Real(_)
  | `Complex(_) =>
    switch (ValueUtil.realBounds(~lower=-1., ~upper=1., a)) {
    | `BothBound
    | `LowerBound
    | `UpperBound
    | `Inside(_) => ValueUtil.mapRealFloat(a, acos)
    | `Outside
    | `Complex(_) => BasicMath.(`Real((Q.of_ints(1, 2), Pi)) - asin(a))
    | `NaN => `NaN
    }
  | `Matrix
  | `NaN => `NaN
  };

let atan = (a: value): value =>
  switch (TrigUtil.compareTrigReal(a)) {
  | `Real((-1), 1, `Sqrt(3)) => realQC(Q.of_ints(-1, 3), Pi)
  | `Real((-1), 1, `Unit) => realQC(Q.of_ints(-1, 4), Pi)
  | `Real((-1), 3, `Sqrt(3)) => realQC(Q.of_ints(-1, 6), Pi)
  | `Zero
  | `Real(0, 1, `Unit) => zero
  | `Real(1, 3, `Sqrt(3)) => realQC(Q.of_ints(1, 6), Pi)
  | `Real(1, 1, `Unit) => realQC(Q.of_ints(1, 4), Pi)
  | `Real(1, 1, `Sqrt(3)) => realQC(Q.of_ints(1, 3), Pi)
  | `Real(_) => ValueUtil.mapRealFloat(a, atan)
  | `Complex(reQ, reC, imQ, imC) =>
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
  | `NaN => `NaN
  };

let sinh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(q, c) => ofFloat(QCUtil.toFloat(q, c)->sinh)
  | `Imag(_)
  | `Complex(_) => BasicMath.((Exp.exp(x) - Exp.exp(- x)) / ofInt(2))
  | _ => `NaN
  };

let cosh = (x: value): value =>
  switch (x) {
  | `Zero => one
  | `Real(q, c) => ofFloat(QCUtil.toFloat(q, c)->cosh)
  | `Imag(_)
  | `Complex(_) => BasicMath.((Exp.exp(x) + Exp.exp(- x)) / ofInt(2))
  | _ => `NaN
  };

let tanh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(q, c) => ofFloat(QCUtil.toFloat(q, c)->tanh)
  | `Imag(_)
  | `Complex(_) =>
    BasicMath.((Exp.exp(x) - Exp.exp(- x)) / (Exp.exp(x) + Exp.exp(- x)))
  | _ => `NaN
  };

let asinh = (x: value): value =>
  switch (x) {
  | `Zero => zero
  | `Real(q, c) => ofFloat(QCUtil.toFloat(q, c)->FloatUtil.asinh)
  | `Imag(_)
  | `Complex(_) => BasicMath.(Exp.log(x + Pow.sqrt(x * x + one)))
  | _ => `NaN
  };

let acosh = (x: value): value =>
  switch (ValueUtil.realBounds(~lower=1.0, x)) {
  | `Inside(f) => realQC(FloatUtil.acosh(f)->Q.of_float, Constant.Unit)
  | `LowerBound => zero
  | `BothBound
  | `UpperBound
  | `Outside
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

let atanh = (x: value): value =>
  switch (ValueUtil.realBounds(~lower=-1.0, ~upper=1.0, x)) {
  | `Inside(f) => realQC(FloatUtil.atanh(f)->Q.of_float, Constant.Unit)
  | `BothBound
  | `LowerBound
  | `UpperBound
  | `Outside
  | `Complex(_) =>
    let two = `Real((Q.of_int(2), Constant.Unit));
    BasicMath.(Exp.log((one + x) / (one - x)) / two);
  | `NaN => `NaN
  };
