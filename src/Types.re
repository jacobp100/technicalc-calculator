module type BasicMath = {
  type t;
  let nan: t;
  let is_nan: t => bool;
  let add: (t, t) => t;
  let sub: (t, t) => t;
  let mul: (t, t) => t;
  let div: (t, t) => t;
  let neg: t => t;
};

module type Scalar = {
  include BasicMath;
  let zero: t;
  let is_zero: t => bool;
  let exp: t => t;
  let sin: t => t;
  let cos: t => t;
  let tan: t => t;
};
