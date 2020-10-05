open AST_Types;

let rec eval = (~context, node: t): Value.t =>
  switch (node) {
  | NaN => `NaN
  | Zero => Value.zero
  | One => Value.one
  | MinusOne => Value.minusOne
  | I => Value.i
  | MinusI => Value.minusI
  | Pi => Value.pi
  | E => Value.e
  | OfInt(a) => Value.ofInt(a)
  | OfFloat(a) => Value.ofFloat(a)
  | OfString(a) => Value.ofString(a)->Belt.Option.getWithDefault(`NaN)
  | OfStringBase(base, a) =>
    Value.ofStringBase(base, a)->Belt.Option.getWithDefault(`NaN)
  | Percent(percent) => evalScalar(~context, percent)->Value.ofPercent
  | Vector(elements) =>
    Value.ofVector(Belt.Array.map(elements, evalScalar(~context)))
  | Matrix({numRows, numColumns, elements}) =>
    Matrix.{
      numRows,
      numColumns,
      elements: Belt.Array.map(elements, evalScalar(~context)),
    }
    ->Value.ofMatrix
  | OfEncoded(a) => Value_Encoding.decode(a)
  | Variable(ident) =>
    Belt.Map.String.getWithDefault(context, ident, Value.nan)
  | Add(a, b) => Value.add(eval(~context, a), eval(~context, b))
  | Sub(a, b) => Value.sub(eval(~context, a), eval(~context, b))
  | Mul(a, b) => Value.mul(eval(~context, a), eval(~context, b))
  | Div(a, b) => Value.div(eval(~context, a), eval(~context, b))
  | Pow(a, b) => Value.pow(eval(~context, a), eval(~context, b))
  | Dot(a, b) => Value.dot(eval(~context, a), eval(~context, b))
  | Neg(a) => Value.neg(eval(~context, a))
  | Abs(a) => Value.abs(eval(~context, a))
  | Floor(a) => Value.floor(eval(~context, a))
  | Ceil(a) => Value.ceil(eval(~context, a))
  | Round(a) => Value.round(eval(~context, a))
  | Sqrt(a) => Value.sqrt(eval(~context, a))
  | Exp(a) => Value.exp(eval(~context, a))
  | Log(a) => Value.log(eval(~context, a))
  | Sin(a) => Value.sin(eval(~context, a))
  | Asin(a) => Value.asin(eval(~context, a))
  | Sinh(a) => Value.sinh(eval(~context, a))
  | Asinh(a) => Value.asinh(eval(~context, a))
  | Cos(a) => Value.cos(eval(~context, a))
  | Acos(a) => Value.acos(eval(~context, a))
  | Cosh(a) => Value.cosh(eval(~context, a))
  | Acosh(a) => Value.acosh(eval(~context, a))
  | Tan(a) => Value.tan(eval(~context, a))
  | Atan(a) => Value.atan(eval(~context, a))
  | Tanh(a) => Value.tanh(eval(~context, a))
  | Atanh(a) => Value.atanh(eval(~context, a))
  | Re(a) => Value.re(eval(~context, a))
  | Im(a) => Value.im(eval(~context, a))
  | Conj(a) => Value.conj(eval(~context, a))
  | Gamma(a) => Value.gamma(eval(~context, a))
  | Factorial(a) => Value.factorial(eval(~context, a))
  | Rand => Value.rand()
  | RandInt(a, b) => Value.randInt(eval(~context, a), eval(~context, b))
  | NPR(a, b) => Value.nPr(eval(~context, a), eval(~context, b))
  | NCR(a, b) => Value.nCr(eval(~context, a), eval(~context, b))
  | Min(a, b) => Value.min(eval(~context, a), eval(~context, b))
  | Max(a, b) => Value.max(eval(~context, a), eval(~context, b))
  | Gcd(a, b) => Value.gcd(eval(~context, a), eval(~context, b))
  | Lcm(a, b) => Value.lcm(eval(~context, a), eval(~context, b))
  | Differential({x, body}) =>
    Value.derivative(createEvalCb(~context, body), eval(~context, x))
  | Integral({a, b, body}) =>
    Value.integrate(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | Sum({a, b, body}) =>
    Value.sum(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | Product({a, b, body}) =>
    Value.product(
      createEvalCb(~context, body),
      eval(~context, a),
      eval(~context, b),
    )
  | Convert({a, fromUnits, toUnits}) =>
    Units.convert(eval(~context, a), ~fromUnits, ~toUnits)
  }
and createEvalCb = (~context, body, x) =>
  eval(~context=Belt.Map.String.set(context, "x", x), body)
and evalScalar = (~context, x): Scalar.t =>
  switch (eval(~context, x)) {
  | #Scalar.t as s => s
  | _ => Scalar.nan
  };

let eval = (~context=Belt.Map.String.empty, v) => eval(~context, v);

let solveRoot = (body, initial) => {
  let fn = value => {
    let context = Belt.Map.String.empty->Belt.Map.String.set("x", value);
    eval(~context, body);
  };

  let initial = eval(initial);
  initial != `NaN ? Value.solveRoot(fn, initial) : `NaN;
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
