include Types.Scalar;

let i: t;
let minusI: t;

let ofFloats: (float, float) => t;
let ofReal: Real.t => t;
let ofImaginary: Real.t => t;
let ofComponents: (Real.t, Real.t) => t;

let isReal: t => bool;
let isImaginary: t => bool;

let toFloat: t => float;
let toFloats: t => (float, float);
let toComponents: t => (Real.t, Real.t);
