let rec eval = (~context, node: ASTTypes.t): Types.value =>
  switch (node) {
  | `NaN => `NaN
  | `Zero => Value.zero
  | `One => Value.one
  | `MinusOne => Value.minusOne
  | `I => Value.i
  | `MinusI => Value.minusI
  | `Pi => Value.pi
  | `E => Value.e
  | `OfInt(a) => Types.ofInt(a)
  | `OfFloat(a) => Types.ofFloat(a)
  | `OfString(a) => Types.ofString(a)
  | `OfStringBase(base, a) => Types.ofStringBase(base, a)
  | `Vector(elements) =>
    Types.vector(elements->Belt.Array.map(evalScalar(~context)))
  | `Matrix({numRows, numColumns, elements}) =>
    Types.matrix(
      numRows,
      numColumns,
      elements->Belt.Array.map(evalScalar(~context)),
    )
  | `OfEncoded(a) => Encoding.decode(a)
  | `Variable(ident) =>
    Belt.Map.String.getWithDefault(context, ident, Types.nan)
  | `Add(a, b) => Value.add(eval(~context, a), eval(~context, b))
  | `Sub(a, b) => Value.sub(eval(~context, a), eval(~context, b))
  | `Mul(a, b) => Value.mul(eval(~context, a), eval(~context, b))
  | `Div(a, b) => Value.div(eval(~context, a), eval(~context, b))
  | `Pow(a, b) => Value.pow(eval(~context, a), eval(~context, b))
  | `Dot(a, b) => Value.dot(eval(~context, a), eval(~context, b))
  | `Neg(a) => Value.neg(eval(~context, a))
  | `Abs(a) => Value.abs(eval(~context, a))
  | `Floor(a) => Value.floor(eval(~context, a))
  | `Ceil(a) => Value.ceil(eval(~context, a))
  | `Round(a) => Value.round(eval(~context, a))
  | `Sqrt(a) => Value.sqrt(eval(~context, a))
  | `Exp(a) => Value.exp(eval(~context, a))
  | `Log(a) => Value.log(eval(~context, a))
  | `Sin(a) => Value.sin(eval(~context, a))
  | `Asin(a) => Value.asin(eval(~context, a))
  | `Sinh(a) => Value.sinh(eval(~context, a))
  | `Asinh(a) => Value.asinh(eval(~context, a))
  | `Cos(a) => Value.cos(eval(~context, a))
  | `Acos(a) => Value.acos(eval(~context, a))
  | `Cosh(a) => Value.cosh(eval(~context, a))
  | `Acosh(a) => Value.acosh(eval(~context, a))
  | `Tan(a) => Value.tan(eval(~context, a))
  | `Atan(a) => Value.atan(eval(~context, a))
  | `Tanh(a) => Value.tanh(eval(~context, a))
  | `Atanh(a) => Value.atanh(eval(~context, a))
  | `Re(a) => Value.re(eval(~context, a))
  | `Im(a) => Value.im(eval(~context, a))
  | `Conj(a) => Value.conj(eval(~context, a))
  | `Gamma(a) => Value.gamma(eval(~context, a))
  | `Factorial(a) => Value.factorial(eval(~context, a))
  | `Rand => Value.rand()
  | `RandInt(a, b) => Value.randInt(eval(~context, a), eval(~context, b))
  | `NPR(a, b) => Value.nPr(eval(~context, a), eval(~context, b))
  | `NCR(a, b) => Value.nCr(eval(~context, a), eval(~context, b))
  | `Differential({x, body}) =>
    NumericEvaluation.derivative(
      createEvalCb(~context, body),
      eval(~context, x),
    )
  | `Integral({a, b, body}) =>
    NumericEvaluation.integrate(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | `Sum({a, b, body}) =>
    NumericEvaluation.sum(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | `Product({a, b, body}) =>
    NumericEvaluation.product(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  }
and createEvalCb = (~context, body, x) =>
  eval(~context=Belt.Map.String.set(context, "x", x), body)
and evalScalar = (~context, x): Types.scalar =>
  switch (eval(~context, x)) {
  | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aX => aX
  | _ => `Real((Q.undef, Constant.Unit))
  };

let eval = (~context=Belt.Map.String.empty, v) => eval(~context, v);

exception Test(bool);
let solveRoot = (body, initial) => {
  let fn = value => {
    let context = Belt.Map.String.empty->Belt.Map.String.set("x", value);
    eval(~context, body);
  };

  let initial = eval(initial);
  initial != `NaN ? Value.steffanRoot(fn, initial) : `NaN;
};
let solveQuadratic = (a, b, c) => {
  let a = eval(a);
  let b = a != `NaN ? eval(b) : `NaN;
  let c = b != `NaN ? eval(c) : `NaN;
  c != `NaN ? Value.quadratic(a, b, c) : (`NaN, `NaN);
};
let solveCubic = (a, b, c, d) => {
  let a = eval(a);
  let b = a != `NaN ? eval(b) : `NaN;
  let c = b != `NaN ? eval(c) : `NaN;
  let d = c != `NaN ? eval(d) : `NaN;
  d != `NaN ? Value.cubic(a, b, c, d) : (`NaN, `NaN, `NaN);
};
let solveVar2 = (x0, y0, c0, x1, y1, c1) => {
  let x0 = eval(x0);
  let y0 = x0 != `NaN ? eval(y0) : `NaN;
  let c0 = y0 != `NaN ? eval(c0) : `NaN;
  let x1 = c0 != `NaN ? eval(x1) : `NaN;
  let y1 = x1 != `NaN ? eval(y1) : `NaN;
  let c1 = y1 != `NaN ? eval(c1) : `NaN;
  c1 != `NaN ? Value.var2(x0, y0, c0, x1, y1, c1) : (`NaN, `NaN);
};
let solveVar3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) => {
  let x0 = eval(x0);
  let y0 = x0 != `NaN ? eval(y0) : `NaN;
  let c0 = y0 != `NaN ? eval(c0) : `NaN;
  let z0 = c0 != `NaN ? eval(z0) : `NaN;
  let x1 = z0 != `NaN ? eval(x1) : `NaN;
  let y1 = x1 != `NaN ? eval(y1) : `NaN;
  let z1 = y1 != `NaN ? eval(z1) : `NaN;
  let c1 = z1 != `NaN ? eval(c1) : `NaN;
  let x2 = c1 != `NaN ? eval(x2) : `NaN;
  let y2 = x2 != `NaN ? eval(y2) : `NaN;
  let z2 = y2 != `NaN ? eval(z2) : `NaN;
  let c2 = z2 != `NaN ? eval(c2) : `NaN;
  c2 != `NaN
    ? Value.var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2)
    : (`NaN, `NaN, `NaN);
};
