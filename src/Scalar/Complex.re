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
let is_nan = a => Real.is_nan(a.re) || Real.is_nan(a.im);

let zero = {re: Real.zero, im: Real.zero};
let is_zero = a => Real.is_zero(a.re) && Real.is_zero(a.im);

let i = {re: Real.zero, im: Real.of_int64(1L)};

let normalize = a => is_nan(a) ? nan : a;

let of_real = a => normalize({re: a, im: Real.zero});
let of_imaginary = a => normalize({re: Real.zero, im: a});
let of_components = (re, im) => normalize({re, im});

let is_real = a => Real.is_zero(a.im);
let is_imaginary = a => Real.is_zero(a.re);

let float_of_complex = a =>
  is_real(a) ? Real.float_of_real(a.re) : Pervasives.nan;

let tuple_of_complex = a => (
  Real.float_of_real(a.re),
  Real.float_of_real(a.im),
);

let _magnitude = a => a.re * a.re + a.im * a.im;

let neg = a => of_components(- a.re, - a.im);

let add = (a, b) => of_components(a.re + b.re, a.im + b.im);

let sub = (a, b) => add(a, neg(b));

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
    *$ of_imaginary(Real.of_int64(~denominator=2L, 1L));
  };

let cos = a =>
  if (is_real(a)) {
    of_real(Real.cos(a.re));
  } else {
    let iA = a *$ i;
    (exp(iA) +$ exp(neg(iA)))
    *$ of_real(Real.of_int64(~denominator=2L, 1L));
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

let to_string = x =>
  Real.to_string(x.re) ++ "+" ++ Real.to_string(x.im) ++ "i";
