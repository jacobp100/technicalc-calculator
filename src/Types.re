module type BasicMath = {
  type t;
  let nan: t;
  let is_nan: t => bool;
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
  let zero: t;
  let one: t;
  let minus_one: t;
  let pi: t;
  let e: t;
  let exp: t => t;
  let sqrt: t => t;
  let log: t => t;
  let sin: t => t;
  let cos: t => t;
  let tan: t => t;
};
