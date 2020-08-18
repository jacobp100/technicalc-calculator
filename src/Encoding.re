open Types;

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

let%private encodeScalar = (a: scalar): scalarEncoding =>
  switch (a) {
  | `Zero => Zero
  | `Real(re) => Real(encodeReal(re))
  | `Imag(im) => Imag(encodeReal(im))
  | `Complex(re, im) => Complex(encodeReal(re), encodeReal(im))
  };

let%private decodeScalar = (a: scalarEncoding): scalar =>
  switch (a) {
  | Zero => `Zero
  | Real(re) => `Real(decodeReal(re))
  | Imag(im) => `Imag(decodeReal(im))
  | Complex(re, im) => `Complex((decodeReal(re), decodeReal(im)))
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

let encode = (a: value): encoding =>
  switch (a) {
  | `Zero => Zero
  | `Real(re) => Real(encodeReal(re))
  | `Imag(im) => Imag(encodeReal(im))
  | `Complex(re, im) => Complex(encodeReal(re), encodeReal(im))
  | `Vector(elements) => Vector(elements->Belt.Array.map(encodeScalar))
  | `Matrix({numRows, numColumns, elements}) =>
    Matrix(numRows, numColumns, elements->Belt.Array.map(encodeScalar))
  | `Percent(p) => Percent(encodeScalar(p))
  | `NaN => NaN
  };

let decode = (a: encoding): value =>
  normalize(
    switch (a) {
    | Zero => `Zero
    | Real(re) => `Real(decodeReal(re))
    | Imag(im) => `Imag(decodeReal(im))
    | Complex(re, im) => `Complex((decodeReal(re), decodeReal(im)))
    | Vector(elements) => `Vector(elements->Belt.Array.map(decodeScalar))
    | Matrix(numRows, numColumns, elements) =>
      let elements = elements->Belt.Array.map(decodeScalar);
      `Matrix({numRows, numColumns, elements});
    | Percent(p) => `Percent(decodeScalar(p))
    | NaN => `NaN
    },
  );
