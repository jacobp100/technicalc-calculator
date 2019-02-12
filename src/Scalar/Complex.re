open PervasivesNoPoly;

let (==) = Real.equal;
let (+) = Real.add;
let (-) = Real.sub;
let ( * ) = Real.mul;
let (/) = Real.div;
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

let equal = (a, b) => a.re == b.re && a.im == b.im;

let i = {re: Real.zero, im: Real.one};
let minus_i = {re: Real.zero, im: Real.minus_one};

let is_nan = a => Real.is_nan(a.re) || Real.is_nan(a.im);

let is_real = a => a.im == Real.zero;
let is_imaginary = a => a.re == Real.zero;

let normalize = a => is_nan(a) ? nan : a;

let of_real = a => normalize({re: a, im: Real.zero});
let of_imaginary = a => normalize({re: Real.zero, im: a});
let of_components = (re, im) => normalize({re, im});

let of_float = f => of_real(Real.of_float(f));
let of_string = s => of_real(Real.of_string(s));
let of_string_base = (base, s) => of_real(Real.of_string_base(base, s));
let of_int = i => of_real(Real.of_int(i));
let of_floats = (re, im) =>
  normalize({re: Real.of_float(re), im: Real.of_float(im)});

let to_float = a => is_real(a) ? Real.to_float(a.re) : Pervasives.nan;
let to_floats = a => (Real.to_float(a.re), Real.to_float(a.im));

let _magnitude2 = a => a.re * a.re + a.im * a.im;
let _arg = ({re: x, im: y}) =>
  if (x == Real.zero) {
    switch (Pervasives.compare(Real.to_float(y), 0.0)) {
    | 1 => Real.pi / Real.of_int(2)
    | (-1) => Real.pi / Real.of_int(-2)
    | _ => Real.nan
    };
  } else {
    Real.of_float(atan2(Real.to_float(y), Real.to_float(x)));
  };

let to_int = a => is_real(a) ? Real.to_int(a.re) : None;

let _format_imaginary = (~format, im) =>
  switch (Real.to_string(~format, im)) {
  | "1" => "i"
  | "-1" => "-i"
  | x => x ++ "i"
  };

let to_string = (~format=OutputFormat.default, x) =>
  if (is_real(x)) {
    Real.to_string(~format, x.re);
  } else if (is_imaginary(x)) {
    _format_imaginary(~format, x.im);
  } else {
    let format = {...format, precision: Pervasives.(/)(format.precision, 3)};
    let re = Real.to_string(~format, x.re);
    let im = _format_imaginary(~format, x.im);
    let (im, op) =
      if (Pervasives.(==)(im.[0], '-')) {
        (String.sub(im, 1, Pervasives.(-)(String.length(im), 1)), "-");
      } else {
        (im, "+");
      };
    re ++ op ++ im;
  };

let neg = a => of_components(- a.re, - a.im);
let abs = a => of_components(Real.abs(a.re), Real.abs(a.im));
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
  if (equal(b, zero)) {
    nan;
  } else if (is_real(a) && is_real(b)) {
    of_real(a.re / b.re);
  } else {
    /* More precision than _magnitude */
    let s = _magnitude2(b);
    let bReciprocal = of_components(b.re / s, - (b.im / s));
    mul(a, bReciprocal);
  };

let (+$) = add;
let (-$) = sub;
let ( *$ ) = mul;
let (/$) = div;
let (~-$) = neg;

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
  if (is_real(a) && Real.to_float(a.re) >% 0.) {
    of_real(Real.log(a.re));
  } else if (is_real(a) && Real.to_float(a.re) ==% (-1.)) {
    of_imaginary(Real.pi);
  } else {
    of_components(Real.log(_magnitude2(a)) / Real.of_int(2), _arg(a));
  };

let pow = (a, b) =>
  if (is_real(a)
      && is_real(b)
      && (Real.is_integer(b.re) || Real.to_float(a.re) >=% 0.)) {
    of_real(Real.pow(a.re, b.re));
  } else if (is_real(a) && a.re == Real.e) {
    let multiplier = Real.exp(b.re);
    of_components(multiplier * Real.cos(b.im), multiplier * Real.sin(b.im));
  } else if (is_real(a)
             && is_real(b)
             && Pervasives.(<)(Real.to_float(a.re), 0.)
             && Pervasives.(==)(Real.to_float(b.re), 0.5)) {
    of_imaginary(Real.sqrt(- a.re));
  } else if (equal(a, zero)) {
    /* b == 0 => NaN case handled in first branch */
    zero;
  } else {
    exp(log(a) *$ b);
  };

let sqrt = a => pow(a, of_real(Real.of_int(1, ~denominator=2)));

let between_one_minus_one = x =>
  if (is_real(x)) {
    let f = Pervasives.abs_float(Real.to_float(x.re));
    f <=% 1.0;
  } else {
    false;
  };

let factorial = n =>
  if (is_real(n) && Real.to_float(n.re) >% 0.) {
    of_real(Real.factorial(n.re));
  } else {
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let (p, g) = (FactorialUtil.p, FactorialUtil.g);
    let x = ref(of_float(p[0]));
    for (i in 1 to Pervasives.(-)(Array.length(p), 1)) {
      let p_i = Real.of_float(p[i]);
      let real = n.re + Real.of_int(i);
      let den = real * real + n.im * n.im;
      x := add(x^, of_components(p_i * real / den, - (p_i * n.im) / den));
    };

    let t = add(n, of_float(g +. 0.5));
    let n = add(n, of_float(0.5));

    let result =
      mul(pow(t, n), of_float(Pervasives.sqrt(2. *. FloatUtil.pi)));

    let r = Real.exp(- t.re);
    let t = of_components(r * Real.cos(- t.im), r * Real.sin(- t.im));

    mul(mul(result, t), x^);
  };

let asin = x =>
  if (between_one_minus_one(x)) {
    of_real(Real.asin(x.re));
  } else {
    ~-$i *$ log(i *$ x +$ sqrt(one -$ x *$ x));
  };
let sinh = x =>
  if (is_real(x)) {
    of_real(Real.sinh(x.re));
  } else {
    (exp(x) -$ exp(~-$x)) /$ of_int(2);
  };
let asinh = x =>
  if (is_real(x)) {
    of_real(Real.asinh(x.re));
  } else {
    log(x +$ sqrt(x *$ x +$ one));
  };
let acos = x =>
  if (between_one_minus_one(x)) {
    of_real(Real.acos(x.re));
  } else {
    of_real(Real.pi / Real.of_int(2)) -$ asin(x);
  };
let cosh = x =>
  if (is_real(x)) {
    of_real(Real.cosh(x.re));
  } else {
    (exp(x) +$ exp(~-$x)) /$ of_int(2);
  };
let acosh = x =>
  if (is_real(x) && Real.to_float(x.re) >=% 1.0) {
    of_real(Real.acosh(x.re));
  } else {
    /* From complex.js library */
    let res = acos(x);
    if (Real.to_float(res.im) <=% 0.) {
      of_components(- res.im, res.re);
    } else {
      of_components(res.im, - res.re);
    };
  };
let atan = x =>
  if (is_real(x)) {
    of_real(Real.atan(x.re));
  } else {
    let {re: a, im: b} = x;
    let c = Real.one - b;
    let d = a * a + c * c;
    let t1 =
      of_components((Real.one - b * b - a * a) / d, Real.of_int(-2) * a / d)
      |> log;
    of_components(- t1.im, t1.re) /$ of_int(2);
  };
let tanh = x =>
  if (is_real(x)) {
    of_real(Real.tanh(x.re));
  } else {
    (exp(x) -$ exp(~-$x)) /$ (exp(x) +$ exp(~-$x));
  };
let atanh = x =>
  if (between_one_minus_one(x)) {
    of_real(Real.atanh(x.re));
  } else {
    log((one +$ x) /$ (one -$ x)) /$ of_int(2);
  };
