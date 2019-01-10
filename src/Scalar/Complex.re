let (+) = Real.add;
let (-) = Real.sub;
let ( * ) = Real.mul;
let (/) = Real.div;
/* let (mod) = Int64.rem; */
let (~-) = Real.neg;

type t = {
  re: Real.t,
  im: Real.t,
};

let nan = {re: Real.nan, im: Real.nan};
let zero = {re: Real.zero, im: Real.zero};
let one = {re: Real.one, im: Real.zero};
let minus_one = {re: Real.minus_one, im: Real.zero};
let pi = {re: Real.pi, im: Real.zero};
let e = {re: Real.e, im: Real.zero};

let i = {re: Real.zero, im: Real.one};
let minus_i = {re: Real.zero, im: Real.minus_one};

let is_nan = a => Real.is_nan(a.re) || Real.is_nan(a.im);
let is_zero = a => Real.is_zero(a.re) && Real.is_zero(a.im);

let is_real = a => Real.is_zero(a.im);
let is_imaginary = a => Real.is_zero(a.re);

let normalize = a => is_nan(a) ? nan : a;

let of_real = a => normalize({re: a, im: Real.zero});
let of_imaginary = a => normalize({re: Real.zero, im: a});
let of_components = (re, im) => normalize({re, im});

let of_float = re => normalize({re: Real.of_float(re), im: Real.zero});
let of_floats = (re, im) =>
  normalize({re: Real.of_float(re), im: Real.of_float(im)});

let to_float = a => is_real(a) ? Real.to_float(a.re) : Pervasives.nan;
let to_floats = a => (Real.to_float(a.re), Real.to_float(a.im));

let _magnitude = a => a.re * a.re + a.im * a.im;

let _arg = a =>
  Real.of_float(atan2(Real.to_float(a.im), Real.to_float(a.re)));

let neg = a => of_components(- a.re, - a.im);

let add = (a, b) => of_components(a.re + b.re, a.im + b.im);

let sub = (a, b) => of_components(a.re - b.re, a.im - b.im);

let mul = (a, b) =>
  if (is_real(a) && is_real(b)) {
    of_real(a.re * b.re);
  } else if (is_imaginary(a) && is_real(b)) {
    of_imaginary(a.im * b.re);
  } else if (is_real(a) && is_imaginary(b)) {
    of_imaginary(a.re * b.im);
  } else if (is_imaginary(a) && is_imaginary(b)) {
    of_real(- (a.im * b.im));
  } else {
    of_components(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re);
  };

let div = (a, b) =>
  if (is_zero(b)) {
    nan;
  } else if (is_real(a) && is_real(b)) {
    of_real(a.re / b.re);
  } else {
    let s = _magnitude(b);
    let bReciprocal = of_components(b.re / s, - (b.im / s));
    mul(a, bReciprocal);
  };

let (+$) = add;
let (-$) = sub;
let ( *$ ) = mul;
let (/$) = div;
/* let (mod) = Int64.rem; */

let exp = a => {
  let exp_part = Real.exp(a.re);
  of_components(exp_part * Real.cos(a.im), exp_part * Real.sin(a.im));
};

let sin = a =>
  if (is_real(a)) {
    of_real(Real.sin(a.re));
  } else {
    let iA = a *$ i;
    (exp(neg(iA)) -$ exp(iA))
    *$ of_imaginary(Real.of_int(1, ~denominator=2));
  };

let cos = a =>
  if (is_real(a)) {
    of_real(Real.cos(a.re));
  } else {
    let iA = a *$ i;
    (exp(iA) +$ exp(neg(iA))) *$ of_real(Real.of_int(1, ~denominator=2));
  };

let tan = a =>
  if (is_real(a)) {
    of_real(Real.tan(a.re));
  } else {
    let iA = a *$ i;
    let x = exp(iA);
    let y = exp(neg(iA));
    (x -$ y) /$ ((x +$ y) *$ i);
  };

let log = a =>
  if (is_real(a) && Real.to_float(a.re) == (-1.)) {
    of_imaginary(Real.pi);
  } else {
    of_components(Real.log(_magnitude(a)), _arg(a));
  };

let pow = (a, b) =>
  if (is_zero(b)) {
    of_real(Real.one);
  } else if (is_real(a) && is_real(b) && Real.to_float(a.re) >= 0.) {
    of_real(Real.pow(a.re, b.re));
  } else if (is_real(a) && is_real(b) && Real.to_float(b.re) == (-0.5)) {
    of_imaginary(Real.sqrt(a.re));
  } else if (is_real(a) && a.re == Real.e) {
    of_components(Real.cos(a.re), Real.sin(a.im));
  } else {
    exp(log(a) *$ b);
  };

let sqrt = a => pow(a, of_real(Real.of_int(1, ~denominator=2)));

let to_string = x =>
  if (is_real(x)) {
    Real.to_string(x.re);
  } else if (is_imaginary(x)) {
    Real.to_string(x.im) ++ "i";
  } else {
    Real.to_string(x.re) ++ "+" ++ Real.to_string(x.im) ++ "i";
  };
