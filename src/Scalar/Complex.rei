include Types.Scalar;

let i: t;
let minus_i: t;

let of_floats: (float, float) => t;
let of_real: Real.t => t;
let of_imaginary: Real.t => t;
let of_components: (Real.t, Real.t) => t;

let is_real: t => bool;
let is_imaginary: t => bool;

let to_float: t => float;
let to_floats: t => (float, float);
let to_components: t => (Real.t, Real.t);
