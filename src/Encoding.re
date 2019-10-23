open Types;

type constantEncoding = [
  | `Unit
  | `Pi
  | `Exp(int)
  | `Sqrt(int)
  | `UnknownValue
];

let encodeConstant = (a: Real_Constant.t): constantEncoding =>
  switch (a) {
  | Unit => `Unit
  | Pi => `Pi
  | Exp(e) => `Exp(e)
  | Sqrt(s) => `Sqrt(s)
  };

let decodeConstant = (a: constantEncoding): Real_Constant.t =>
  switch (a) {
  | `Pi => Pi
  | `Exp(e) => Exp(e)
  | `Sqrt(s) => Sqrt(s)
  | _ => Unit
  };

type realEncoding = [
  | `Rational(int, int, constantEncoding)
  | `Float(string)
];

let encodeReal = (a: Real.t): realEncoding =>
  switch (a) {
  | Rational(n, d, c) => `Rational((n, d, encodeConstant(c)))
  | Float(f) => `Float(string_of_float(f))
  };

let decodeReal = (a: realEncoding): Real.t =>
  switch (a) {
  | `Rational(n, d, c) => Rational(n, d, decodeConstant(c))
  | `Float(f) => Float(float_of_string(f))
  };

type scalarEncoding = [
  | `Zero
  | `Real(realEncoding)
  | `Imag(realEncoding)
  | `Complex(realEncoding, realEncoding)
];

let encodeScalar = (a: scalar): scalarEncoding =>
  switch (a) {
  | `Zero => `Zero
  | `Real(re) => `Real(encodeReal(re))
  | `Imag(im) => `Imag(encodeReal(im))
  | `Complex(re, im) => `Complex((encodeReal(re), encodeReal(im)))
  };

let decodeScalar = (a: scalarEncoding): scalar =>
  switch (a) {
  | `Zero => `Zero
  | `Real(re) => `Real(decodeReal(re))
  | `Imag(im) => `Imag(decodeReal(im))
  | `Complex(re, im) => `Complex((decodeReal(re), decodeReal(im)))
  };

type vectorEncoding = [ | `Vector(array(scalarEncoding))];
type matrixEncoding = [ | `Matrix(Matrix.t(scalarEncoding))];
type percentEncoding = [ | `Percent(scalarEncoding)];

type encoding = [
  scalarEncoding
  | vectorEncoding
  | matrixEncoding
  | percentEncoding
  | `NaN
];

external scalarEncodingToEncoding: scalarEncoding => encoding = "%identity";
external matrixEncodingToEncoding: matrixEncoding => encoding = "%identity";
external percentEncodingToEncoding: percentEncoding => encoding = "%identity";

let encode = (a: value): encoding =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV =>
    encodeScalar(aV)->scalarEncodingToEncoding
  | `Vector(elements) => `Vector(elements->Belt.Array.map(encodeScalar))
  | `Matrix(mat) => `Matrix(mat->Matrix.map(encodeScalar))
  | `Percent(p) => `Percent(encodeScalar(p))
  | `NaN => `NaN
  };

let decode = (a: encoding): value =>
  switch (a) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV =>
    decodeScalar(aV)->valueOfScalar
  | `Vector(elements) => `Vector(elements->Belt.Array.map(decodeScalar))
  | `Matrix(mat) => `Matrix(mat->Matrix.map(decodeScalar))
  | `Percent(p) => `Percent(decodeScalar(p))
  | `NaN => `NaN
  };