module type BasicMath = {
  type t;
  let nan: t;
  let is_nan: t => bool;
  let to_string: (~format: OutputFormat.format=?, t) => string;
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
  let of_int: int => t;
  let of_float: float => t;
  let of_string: string => t;
  let of_string_base: (int, string) => t;
  let to_int: t => option(int);
  let zero: t;
  let one: t;
  let minus_one: t;
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
  let of_matrix_elements: (~rows: int, ~columns: int, array(t)) => t;
  let to_int: t => option(int);

  let dot: (t, t) => t;
};

module type MakeValue =
  (Number: Scalar) =>
   {
    include BaseValue;
    let of_number: Number.t => t;
    let to_number: t => option(Number.t);
  };
