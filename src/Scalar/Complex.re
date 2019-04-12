open PervasivesNoPoly;
open OptChain;

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
let minusOne = {re: Real.minusOne, im: Real.zero};
let pi = {re: Real.pi, im: Real.zero};
let e = {re: Real.e, im: Real.zero};

let equal = (a, b) => a.re == b.re && a.im == b.im;

let i = {re: Real.zero, im: Real.one};
let minusI = {re: Real.zero, im: Real.minusOne};

let isNan = a => Real.isNan(a.re) || Real.isNan(a.im);

let isReal = a => a.im == Real.zero;
let isImaginary = a => a.re == Real.zero;

let normalize = a => isNan(a) ? nan : a;

let ofReal = a => normalize({re: a, im: Real.zero});
let ofImaginary = a => normalize({re: Real.zero, im: a});
let ofComponents = (re, im) => normalize({re, im});

let ofFloat = f => ofReal(Real.ofFloat(f));
let ofString = s => ofReal(Real.ofString(s));
let ofStringBase = (base, s) => ofReal(Real.ofStringBase(base, s));
let ofInt = i => ofReal(Real.ofInt(i));
let ofFloats = (re, im) =>
  normalize({re: Real.ofFloat(re), im: Real.ofFloat(im)});

let toComponents = a => (a.re, a.im);
let toFloat = a => isReal(a) ? Real.toFloat(a.re) : Pervasives.nan;
let toFloats = a => (Real.toFloat(a.re), Real.toFloat(a.im));

let toInt = a => isReal(a) ? Real.toInt(a.re) : None;

let _magnitude2 = a => a.re * a.re + a.im * a.im;
let _arg = ({re: x, im: y}) =>
  if (x == Real.zero) {
    switch (Pervasives.compare(Real.toFloat(y), 0.0)) {
    | 1 => Real.ofInt(1, ~denominator=2, ~constant=Pi)
    | (-1) => Real.ofInt(-1, ~denominator=2, ~constant=Pi)
    | _ => Real.nan
    };
  } else {
    Real.ofFloat(atan2(Real.toFloat(y), Real.toFloat(x)));
  };
let _classify = a =>
  switch (a.re == Real.zero, a.im == Real.zero) {
  | (true, true) => `Zero
  | (false, true) => `Real
  | (true, false) => `Imaginary
  | (false, false) => `Complex
  };
let _sign = a =>
  switch (
    a.re == Real.zero ? 0 : Real.toFloat(a.re) >% 0. ? 1 : (-1),
    a.im == Real.zero ? 0 : Real.toFloat(a.im) >% 0. ? 1 : (-1),
  ) {
  | (0, 0) => `Zero
  | (1, 0) => `Positive
  | ((-1), 0) => `Negative
  | (0, 1) => `PositiveI
  | (0, (-1)) => `NegativeI
  | _ => `Undefined
  };
let _bounds = (~lower=?, ~upper=?, x) =>
  if (isReal(x)) {
    FloatUtil.bounds(~lower?, ~upper?, Real.toFloat(x.re));
  } else {
    `Outside;
  };

let _formatImaginary = (~format: OutputFormat.format, im) => {
  let specialValue =
    if (im == Real.one) {
      `One;
    } else if (im == Real.minusOne) {
      `MinusOne;
    } else {
      `Other;
    };

  switch (format.mode, specialValue) {
  | (String | Tex, `One) => "i"
  | (String | Tex, `MinusOne) => "-i"
  | (String | Tex, `Other) => Real.toString(~format, im) ++ "i"
  | (MathML, `One) => "<mi>i</mi>"
  | (MathML, `MinusOne) => "<mo>-</mo><mi>i</mi>"
  | (MathML, `Other) => Real.toString(~format, im) ++ "<mi>i</mi>"
  };
};

let toString = (~format=OutputFormat.default, x) =>
  switch (_classify(x)) {
  | `Real
  | `Zero => Real.toString(~format, x.re)
  | `Imaginary => _formatImaginary(~format, x.im)
  | `Complex =>
    let format = {...format, precision: Pervasives.(/)(format.precision, 3)};
    let re = Real.toString(~format, x.re);
    let im = _formatImaginary(~format, Real.abs(x.im));
    let op =
      switch (format.mode, !Real.isNegative(x.im)) {
      | (String | Tex, true) => "+"
      | (String | Tex, false) => "-"
      | (MathML, true) => "<mo>+</mo>"
      | (MathML, false) => "<mo>-</mo>"
      };
    re ++ op ++ im;
  };

let neg = a => ofComponents(- a.re, - a.im);
let abs = a =>
  ofReal(isReal(a) ? Real.abs(a.re) : Real.sqrt(_magnitude2(a)));
let add = (a, b) => ofComponents(a.re + b.re, a.im + b.im);
let sub = (a, b) => ofComponents(a.re - b.re, a.im - b.im);

let mul = (a, b) =>
  switch (_classify(a), _classify(b)) {
  | (`Zero, _)
  | (_, `Zero) => zero
  | (`Real, `Real) => ofReal(a.re * b.re)
  | (`Real, `Imaginary) => ofImaginary(a.re * b.im)
  | (`Imaginary, `Real) => ofImaginary(a.im * b.re)
  | (`Imaginary, `Imaginary) => ofReal(- (a.im * b.im))
  | _ => ofComponents(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re)
  };

let div = (a, b) =>
  switch (_classify(a), _classify(b)) {
  | (_, `Zero) => nan
  | (`Zero, _) => zero
  | (_, `Real) => ofComponents(a.re / b.re, a.im / b.re)
  | (_, `Imaginary) => ofComponents(a.im / b.im, - a.re / b.im)
  | (`Real, _) =>
    let s = _magnitude2(b);
    ofComponents(a.re * b.re / s, - a.re * b.im / s);
  | (`Imaginary, _) =>
    let s = _magnitude2(b);
    ofComponents(a.im * b.im / s, a.im * b.re / s);
  | _ =>
    let s = _magnitude2(b);
    let bReciprocal = ofComponents(b.re / s, - b.im / s);
    mul(a, bReciprocal);
  };

let (+$) = add;
let (-$) = sub;
let ( *$ ) = mul;
let (/$) = div;
let (~-$) = neg;

let exp = a => {
  let expPart = Real.exp(a.re);
  ofComponents(expPart * Real.cos(a.im), expPart * Real.sin(a.im));
};

let sin = a =>
  if (isReal(a)) {
    ofReal(Real.sin(a.re));
  } else {
    let iA = a *$ i;
    (exp(neg(iA)) -$ exp(iA))
    *$ ofImaginary(Real.ofInt(1, ~denominator=2));
  };

let cos = a =>
  if (isReal(a)) {
    ofReal(Real.cos(a.re));
  } else {
    let iA = a *$ i;
    (exp(iA) +$ exp(neg(iA))) *$ ofReal(Real.ofInt(1, ~denominator=2));
  };

let tan = a =>
  if (isReal(a)) {
    ofReal(Real.tan(a.re));
  } else {
    let iA = a *$ i;
    let x = exp(iA);
    let y = exp(neg(iA));
    (x -$ y) /$ ((x +$ y) *$ i);
  };

let log = a =>
  switch (_sign(a)) {
  | `Positive => ofReal(Real.log(a.re))
  | `Zero => nan
  | `Negative when a.re == Real.minusOne => ofImaginary(Real.pi)
  | _ => ofComponents(Real.log(_magnitude2(a)) / Real.ofInt(2), _arg(a))
  };

let pow = (a, b) =>
  switch (_sign(a), _sign(b)) {
  | (`Zero, `Zero) => nan
  | (_, `Zero) => one
  | (`Zero, _) => zero
  | (`Positive, `Positive | `Negative) => ofReal(Real.pow(a.re, b.re))
  | (`Negative, `Positive) when Real.isInt(b.re) =>
    ofReal(Real.pow(a.re, b.re))
  | (`Positive, _) when a.re == Real.e => exp(b)
  | (`Negative, `Positive) when b.re == Real.ofInt(1, ~denominator=2) =>
    ofImaginary(Real.sqrt(- a.re))
  | (`PositiveI | `NegativeI, `Positive | `Negative) when Real.isInt(b.re) =>
    let aPowB = Real.pow(a.im, b.re);
    switch (b.re->Real.toZ |? ZUtil.safeMod(_, Z.of_int(4)) |? Z.to_int) {
    | Some(0) => ofReal(aPowB)
    | Some(1) => ofImaginary(aPowB)
    | Some(2) => ofReal(aPowB)->neg
    | Some(3) => ofImaginary(aPowB)->neg
    | _ => raise(Not_found)
    };
  | _ => exp(log(a) *$ b)
  };

let sqrt = a => pow(a, ofReal(Real.ofInt(1, ~denominator=2)));

let factorial = n =>
  switch (_sign(n)) {
  | `Zero
  | `Positive => ofReal(Real.factorial(n.re))
  | _ =>
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let (p, g) = (FactorialUtil.p, FactorialUtil.g);
    let x = ref(ofFloat(p[0]));
    for (i in 1 to Pervasives.(-)(Belt.Array.length(p), 1)) {
      let pI = Real.ofFloat(p[i]);
      let real = n.re + Real.ofInt(i);
      let den = real * real + n.im * n.im;
      x := add(x^, ofComponents(pI * real / den, - (pI * n.im) / den));
    };

    let t = add(n, ofFloat(g +. 0.5));
    let n = add(n, ofFloat(0.5));

    let result =
      mul(pow(t, n), ofFloat(Pervasives.sqrt(2. *. FloatUtil.pi)));

    let r = Real.exp(- t.re);
    let t = ofComponents(r * Real.cos(- t.im), r * Real.sin(- t.im));

    mul(mul(result, t), x^);
  };

let asin = x =>
  switch (_bounds(~lower=-1., ~upper=1., x)) {
  | `Outside => ~-$i *$ log(i *$ x +$ sqrt(one -$ x *$ x))
  | _ => ofReal(Real.asin(x.re))
  };
let sinh = x =>
  if (isReal(x)) {
    ofReal(Real.sinh(x.re));
  } else {
    (exp(x) -$ exp(~-$x)) /$ ofInt(2);
  };
let asinh = x =>
  if (isReal(x)) {
    ofReal(Real.asinh(x.re));
  } else {
    log(x +$ sqrt(x *$ x +$ one));
  };
let acos = x =>
  switch (_bounds(~lower=-1., ~upper=1., x)) {
  | `Outside =>
    ofReal(Real.ofInt(1, ~denominator=2, ~constant=Pi)) -$ asin(x)
  | _ => ofReal(Real.acos(x.re))
  };
let cosh = x =>
  if (isReal(x)) {
    ofReal(Real.cosh(x.re));
  } else {
    (exp(x) +$ exp(~-$x)) /$ ofInt(2);
  };
let acosh = x =>
  if (isReal(x) && Real.toFloat(x.re) >=% 1.0) {
    ofReal(Real.acosh(x.re));
  } else {
    /* From complex.js library */
    let res = acos(x);
    if (Real.toFloat(res.im) <=% 0.) {
      ofComponents(- res.im, res.re);
    } else {
      ofComponents(res.im, - res.re);
    };
  };
let atan = x =>
  if (isReal(x)) {
    ofReal(Real.atan(x.re));
  } else {
    let {re: a, im: b} = x;
    let c = Real.one - b;
    let d = a * a + c * c;
    let t1 =
      ofComponents((Real.one - b * b - a * a) / d, Real.ofInt(-2) * a / d)
      ->log;
    ofComponents(- t1.im, t1.re) /$ ofInt(2);
  };
let tanh = x =>
  if (isReal(x)) {
    ofReal(Real.tanh(x.re));
  } else {
    (exp(x) -$ exp(~-$x)) /$ (exp(x) +$ exp(~-$x));
  };
let atanh = x =>
  switch (_bounds(~lower=-1., ~upper=1., x)) {
  | `Outside => log((one +$ x) /$ (one -$ x)) /$ ofInt(2)
  | _ => ofReal(Real.atanh(x.re))
  };
