open Types;

type constantEncoding = [
  | `Unit
  | `Pi
  | `Exp(string)
  | `Sqrt(string)
  | `UnknownValue
];

let encodeConstant = (a: Constant.t): constantEncoding =>
  switch (a) {
  | Unit => `Unit
  | Pi => `Pi
  | Exp(e) => `Exp(string_of_int(e))
  | Sqrt(s) => `Sqrt(Z.to_string(s))
  };

let decodeConstant = (a: constantEncoding): Constant.t =>
  switch (a) {
  | `Pi => Pi
  | `Exp(e) => Exp(int_of_string(e))
  | `Sqrt(s) => Sqrt(Z.of_string(s))
  | _ => Unit
  };

type scalarEncoding = [
  | `Zero
  | `Real(string, constantEncoding)
  | `Imag(string, constantEncoding)
  | `Complex(string, constantEncoding, string, constantEncoding)
];

let encodeScalar = (a: scalar): scalarEncoding =>
  switch (a) {
  | `Zero => `Zero
  | `Real(q, c) => `Real((Q.to_string(q), encodeConstant(c)))
  | `Imag(q, c) => `Imag((Q.to_string(q), encodeConstant(c)))
  | `Complex(reQ, reC, imQ, imC) =>
    `Complex((
      Q.to_string(reQ),
      encodeConstant(reC),
      Q.to_string(imQ),
      encodeConstant(imC),
    ))
  };

let decodeScalar = (a: scalarEncoding): scalar =>
  switch (a) {
  | `Zero => `Zero
  | `Real(q, c) => `Real((Q.of_string(q), decodeConstant(c)))
  | `Imag(q, c) => `Imag((Q.of_string(q), decodeConstant(c)))
  | `Complex(reQ, reC, imQ, imC) =>
    `Complex((
      Q.of_string(reQ),
      decodeConstant(reC),
      Q.of_string(imQ),
      decodeConstant(imC),
    ))
  };

type matrixEncoding = MatrixTypes.matrixBase(scalarEncoding);

type encoding = [ scalarEncoding | matrixEncoding | `NaN];

external scalarEncodingToEncoding: scalarEncoding => encoding = "%identity";
external matrixEncodingToEncoding: matrixEncoding => encoding = "%identity";

let encode = (a: value): encoding =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV =>
    encodeScalar(aV)->scalarEncodingToEncoding
  | (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as aM =>
    MatrixTypes.mapAny(aM, encodeScalar)->matrixEncodingToEncoding
  | `NaN => `NaN
  };

let decode = (a: encoding): value =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV =>
    decodeScalar(aV)->valueOfScalar
  | (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as aM =>
    MatrixTypes.mapAny(aM, decodeScalar)->valueOfMatrix
  | `NaN => `NaN
  };
