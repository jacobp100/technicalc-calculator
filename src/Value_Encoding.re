open Value_Types;
open Value_Base;

type realEncoding;

let%private encodeReal = (a: Real.t): realEncoding =>
  switch (a) {
  | Rational(n, d, Unit) => Obj.magic([|n, d, 0|])
  | Rational(n, d, Pi) => Obj.magic([|n, d, 1|])
  | Rational(n, d, Exp(i)) => Obj.magic([|n, d, 2, i|])
  | Rational(n, d, Sqrt(i)) => Obj.magic([|n, d, 3, i|])
  | Decimal(f) => Obj.magic(Decimal.toString(f))
  };

let%private decodeReal = (a: realEncoding): Real.t =>
  if (Js.typeof(a) == "string") {
    Decimal(Decimal.ofString(Obj.magic(a)));
  } else if (Js.Array.isArray(a)) {
    switch (Obj.magic(a)) {
    | [|n, d, 0|] => Rational(n, d, Unit)
    | [|n, d, 1|] => Rational(n, d, Pi)
    | [|n, d, 2, i|] => Rational(n, d, Exp(i))
    | [|n, d, 3, i|] => Rational(n, d, Sqrt(i))
    | _ => Real.nan
    };
  } else {
    Real.nan;
  };

type scalarEncoding =
  | Zero
  | Real(realEncoding)
  | Imag(realEncoding)
  | Complex(realEncoding, realEncoding);

let%private encodeScalar = (a: Scalar.t): scalarEncoding =>
  switch (a) {
  | `Z => Zero
  | `R(re) => Real(encodeReal(re))
  | `I(im) => Imag(encodeReal(im))
  | `C(re, im) => Complex(encodeReal(re), encodeReal(im))
  };

let%private decodeScalar = (a: scalarEncoding): Scalar.t =>
  switch (a) {
  | Zero => `Z
  | Real(re) => `R(decodeReal(re))
  | Imag(im) => `I(decodeReal(im))
  | Complex(re, im) => `C((decodeReal(re), decodeReal(im)))
  };

type encoding =
  | Zero
  | Real(realEncoding)
  | Imag(realEncoding)
  | Complex(realEncoding, realEncoding)
  | Vector(array(scalarEncoding))
  | Matrix(int, int, array(scalarEncoding))
  | Percent(scalarEncoding)
  | NaN;

let encode = (a: t): encoding =>
  switch (a) {
  | `Z => Zero
  | `R(re) => Real(encodeReal(re))
  | `I(im) => Imag(encodeReal(im))
  | `C(re, im) => Complex(encodeReal(re), encodeReal(im))
  | `V(elements) => Vector(elements->Belt.Array.map(encodeScalar))
  | `M({numRows, numColumns, elements}) =>
    Matrix(numRows, numColumns, elements->Belt.Array.map(encodeScalar))
  | `P(p) => Percent(encodeScalar(p))
  | `N => NaN
  };

let decode = (a: encoding): t =>
  switch (a) {
  | Zero => `Z
  | Real(re) => ofReal(decodeReal(re))
  | Imag(im) => ofImag(decodeReal(im))
  | Complex(re, im) => ofComplex(decodeReal(re), decodeReal(im))
  | Vector(vector) => ofVector(Belt.Array.map(vector, decodeScalar))
  | Matrix(numRows, numColumns, elements) =>
    let elements = elements->Belt.Array.map(decodeScalar);
    ofMatrix(Matrix.{numRows, numColumns, elements});
  | Percent(p) => `P(decodeScalar(p))
  | NaN => `N
  };
