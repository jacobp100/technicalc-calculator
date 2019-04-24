type range('t) = {
  a: 't,
  b: 't,
  body: 't,
};

type derivative('t) = {
  x: 't,
  body: 't,
};

type t = [
  MatrixTypes.matrixBase(t)
  | `Zero
  | `One
  | `MinusOne
  | `I
  | `MinusI
  | `Pi
  | `E
  | `OfInt(int)
  | `OfFloat(float)
  | `OfString(string)
  | `OfStringBase(int, string)
  | `OfEncoded(Encoding.encoding)
  | `Variable(string)
  | `Add(t, t)
  | `Sub(t, t)
  | `Mul(t, t)
  | `Div(t, t)
  | `Pow(t, t)
  | `Dot(t, t)
  | `Neg(t)
  | `Abs(t)
  | `Sqrt(t)
  | `Exp(t)
  | `Log(t)
  | `Sin(t)
  | `Asin(t)
  | `Sinh(t)
  | `Asinh(t)
  | `Cos(t)
  | `Acos(t)
  | `Cosh(t)
  | `Acosh(t)
  | `Tan(t)
  | `Atan(t)
  | `Tanh(t)
  | `Atanh(t)
  | `Gamma(t)
  | `Factorial(t)
  | `Sum(range(t))
  | `Product(range(t))
  | `Derivative(derivative(t))
];

module Context = Map.Make(String);

let zero = `Zero;
let one = `One;
let minusOne = `MinusOne;
let i = `I;
let minusI = `MinusI;
let pi = `Pi;
let e = `E;
let ofInt = a: t => `OfInt(a);
let ofFloat = a: t => `OfFloat(a);
let ofString = a: t => `OfString(a);
let ofStringBase = (base, a): t => `OfStringBase((base, a));
let ofEncoded = a: t => `OfEncoded(a);
let vector2 = (a, b): t => `Vector2((a, b));
let vector3 = (a, b, c): t => `Vector3((a, b, c));
let matrix2 = (a, b, c, d): t => `Matrix2((a, b, c, d));
let matrix3 = (a, b, c, d, e, f, g, h, i): t =>
  `Matrix3((a, b, c, d, e, f, g, h, i));
let variable = name: t => `Variable(name);
let add = (a, b): t => `Add((a, b));
let sub = (a, b): t => `Sub((a, b));
let mul = (a, b): t => `Mul((a, b));
let div = (a, b): t => `Div((a, b));
let pow = (a, b): t => `Pow((a, b));
let dot = (a, b): t => `Dot((a, b));
let neg = a: t => `Neg(a);
let abs = a: t => `Abs(a);
let sqrt = a: t => `Sqrt(a);
let exp = a: t => `Exp(a);
let log = a: t => `Log(a);
let sin = a: t => `Sin(a);
let asin = a: t => `Asin(a);
let sinh = a: t => `Sinh(a);
let asinh = a: t => `Asinh(a);
let cos = a: t => `Cos(a);
let acos = a: t => `Acos(a);
let cosh = a: t => `Cosh(a);
let acosh = a: t => `Acosh(a);
let tan = a: t => `Tan(a);
let atan = a: t => `Atan(a);
let tanh = a: t => `Tanh(a);
let atanh = a: t => `Atanh(a);
let gamma = a: t => `Gamma(a);
let factorial = a: t => `Factorial(a);
let sum = (a, b, body): t => `Sum({a, b, body});
let product = (a, b, body): t => `Product({a, b, body});
let derivative = (x, b): t => `Derivative({x, body: b});

let rec eval = (~context, node: t): Types.value =>
  switch (node) {
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
  | `OfStringBase(_) => Types.nan
  | (`Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) as aM =>
    MatrixUtil.flatMap(aM, s => eval(~context, s))
  | `OfEncoded(a) => Encoding.decode(a)
  | `Variable(ident) =>
    switch (Context.find(ident, context)) {
    | v => v
    | exception _ => Types.nan
    }
  | `Add(a, b) => Value.add(eval(~context, a), eval(~context, b))
  | `Sub(a, b) => Value.sub(eval(~context, a), eval(~context, b))
  | `Mul(a, b) => Value.mul(eval(~context, a), eval(~context, b))
  | `Div(a, b) => Value.div(eval(~context, a), eval(~context, b))
  | `Pow(a, b) => Value.pow(eval(~context, a), eval(~context, b))
  | `Dot(a, b) => Value.dot(eval(~context, a), eval(~context, b))
  | `Neg(a) => Value.neg(eval(~context, a))
  | `Abs(a) => Value.abs(eval(~context, a))
  | `Sqrt(a) => Value.sqrt(eval(~context, a))
  | `Exp(a) => Value.exp(eval(~context, a))
  | `Log(a) => Value.log(eval(~context, a))
  | `Gamma(a) => Value.gamma(eval(~context, a))
  | `Factorial(a) => Value.factorial(eval(~context, a))
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
  | `Derivative({x, body}) =>
    NumericEvaluation.derivative(
      createEvalCb(~context, body),
      eval(~context, x),
    )
  }
and createEvalCb = (~context, body, x) =>
  eval(~context=Context.add("x", x, context), body);

let eval = (~context=Context.empty, v) => eval(~context, v);

let solveQuadratic = (a, b, c) => {
  let a = eval(a);
  let b = a != `NaN ? eval(b) : `NaN;
  let c = b != `NaN ? eval(c) : `NaN;
  c != `NaN ? Equation.quadratic(a, b, c) : (`NaN, `NaN);
};
let solveCubic = (a, b, c, d) => {
  let a = eval(a);
  let b = a != `NaN ? eval(b) : `NaN;
  let c = b != `NaN ? eval(c) : `NaN;
  let d = c != `NaN ? eval(d) : `NaN;
  d != `NaN ? Equation.cubic(a, b, c, d) : (`NaN, `NaN, `NaN);
};
let solveVar2 = (x0, y0, c0, x1, y1, c1) => {
  let x0 = eval(x0);
  let y0 = x0 != `NaN ? eval(y0) : `NaN;
  let c0 = y0 != `NaN ? eval(c0) : `NaN;
  let x1 = c0 != `NaN ? eval(x1) : `NaN;
  let y1 = x1 != `NaN ? eval(y1) : `NaN;
  let c1 = y1 != `NaN ? eval(c1) : `NaN;
  c1 != `NaN ? Equation.var2(x0, y0, c0, x1, y1, c1) : (`NaN, `NaN);
};
let solveVar3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) => {
  let x0 = eval(x0);
  let y0 = x0 != `NaN ? eval(y0) : `NaN;
  let c0 = y0 != `NaN ? eval(c0) : `NaN;
  let x1 = c0 != `NaN ? eval(x1) : `NaN;
  let y1 = x1 != `NaN ? eval(y1) : `NaN;
  let c1 = y1 != `NaN ? eval(c1) : `NaN;
  let x2 = c1 != `NaN ? eval(x2) : `NaN;
  let y2 = x2 != `NaN ? eval(y2) : `NaN;
  let c2 = y2 != `NaN ? eval(c2) : `NaN;
  c2 != `NaN ?
    Equation.var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) :
    (`NaN, `NaN, `NaN);
};
