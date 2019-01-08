include Types.Scalar;

let of_real: Real.t => t;
let of_imaginary: Real.t => t;
let of_components: (Real.t, Real.t) => t;

let is_real: t => bool;
let is_imaginary: t => bool;

let float_of_complex: t => float;
