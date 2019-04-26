type differential('t) = {
  x: 't,
  body: 't,
};

type integral('t) = {
  a: 't,
  b: 't,
  body: 't,
};

type range('t) = {
  a: 't,
  b: 't,
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
  | `Re(t)
  | `Im(t)
  | `Conj(t)
  | `Gamma(t)
  | `Factorial(t)
  | `Rand
  | `RandInt(t, t)
  | `NPR(t, t)
  | `NCR(t, t)
  | `Differential(differential(t))
  | `Integral(integral(t))
  | `Sum(range(t))
  | `Product(range(t))
];

let zero = `Zero;
let one = `One;
let minusOne = `MinusOne;
let i = `I;
let minusI = `MinusI;
let pi = `Pi;
let e = `E;
let ofInt = (a): t => `OfInt(a);
let ofFloat = (a): t => `OfFloat(a);
let ofString = (a): t => `OfString(a);
let ofStringBase = (base, a): t => `OfStringBase((base, a));
let ofEncoded = (a): t => `OfEncoded(a);
let ofValue = (a): t => `OfEncoded(Encoding.encode(a));
let vector2 = (a, b): t => `Vector2((a, b));
let vector3 = (a, b, c): t => `Vector3((a, b, c));
let matrix2 = (a, b, c, d): t => `Matrix2((a, b, c, d));
let matrix3 = (a, b, c, d, e, f, g, h, i): t =>
  `Matrix3((a, b, c, d, e, f, g, h, i));
let variable = (name): t => `Variable(name);
let add = (a, b): t => `Add((a, b));
let sub = (a, b): t => `Sub((a, b));
let mul = (a, b): t => `Mul((a, b));
let div = (a, b): t => `Div((a, b));
let pow = (a, b): t => `Pow((a, b));
let dot = (a, b): t => `Dot((a, b));
let neg = (a): t => `Neg(a);
let abs = (a): t => `Abs(a);
let sqrt = (a): t => `Sqrt(a);
let exp = (a): t => `Exp(a);
let log = (a): t => `Log(a);
let sin = (a): t => `Sin(a);
let asin = (a): t => `Asin(a);
let sinh = (a): t => `Sinh(a);
let asinh = (a): t => `Asinh(a);
let cos = (a): t => `Cos(a);
let acos = (a): t => `Acos(a);
let cosh = (a): t => `Cosh(a);
let acosh = (a): t => `Acosh(a);
let tan = (a): t => `Tan(a);
let atan = (a): t => `Atan(a);
let tanh = (a): t => `Tanh(a);
let atanh = (a): t => `Atanh(a);
let re = (a): t => `Re(a);
let im = (a): t => `Im(a);
let conj = (a): t => `Conj(a);
let gamma = (a): t => `Gamma(a);
let factorial = (a): t => `Factorial(a);
let rand = (): t => `Rand;
let randInt = (a, b): t => `RandInt((a, b));
let nPr = (a, b): t => `NPR((a, b));
let nCr = (a, b): t => `NCR((a, b));
let differential = (x, body): t => `Differential({x, body});
let integral = (a, b, body): t => `Integral({a, b, body});
let sum = (a, b, body): t => `Sum({a, b, body});
let product = (a, b, body): t => `Product({a, b, body});
