module type BasicMath = {
  type t;
  type encoding;
  let nan: t;
  let isNan: t => bool;
  let toString: (~format: OutputFormat.format=?, t) => string;
  let encode: t => encoding;
  let decode: encoding => t;
  let equal: (t, t) => bool;
  let add: (t, t) => t;
  let sub: (t, t) => t;
  let mul: (t, t) => t;
  let div: (t, t) => t;
  let pow: (t, t) => t;
  let neg: t => t;
};

module type Scalar = {
  include BasicMath;
  let ofInt: int => t;
  let ofFloat: float => t;
  let ofString: string => t;
  let ofStringBase: (int, string) => t;
  let toInt: t => option(int);
  let zero: t;
  let one: t;
  let minusOne: t;
  let pi: t;
  let e: t;
  let abs: t => t;
  let sqrt: t => t;
  let exp: t => t;
  let log: t => t;
  let factorial: t => t;
  let sin: t => t;
  let asin: t => t;
  let sinh: t => t;
  let asinh: t => t;
  let cos: t => t;
  let acos: t => t;
  let cosh: t => t;
  let acosh: t => t;
  let tan: t => t;
  let atan: t => t;
  let tanh: t => t;
  let atanh: t => t;
};

module type BaseValue = {
  include Scalar;
  let ofMatrixElements: (~rows: int, ~columns: int, array(t)) => t;
  let toInt: t => option(int);

  let dot: (t, t) => t;
};

module type MakeValue =
  (Number: Scalar) =>
   {
    include BaseValue;
    let ofNumber: Number.t => t;
    let toNumber: t => option(Number.t);
  };
