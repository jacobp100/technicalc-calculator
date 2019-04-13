module SciLineValue = Value.Make(Complex);
module NumberMatrix = SciLineValue.NumberMatrix;
module SciLineAst = AST.Make(SciLineValue);

module Result =
  FFITypeCheck.Make({
    let key = "resolved";
    type t = SciLineValue.t;
  });

include SciLineAst;

let zero = ofT(SciLineValue.zero);
let one = ofT(SciLineValue.one);
let minusOne = ofT(SciLineValue.minusOne);
let pi = ofT(SciLineValue.pi);
let e = ofT(SciLineValue.e);
let i = ofT(SciLineValue.ofNumber(Complex.i));
let minusI = ofT(SciLineValue.ofNumber(Complex.minusI));

let _convertContext = jsContext =>
  Js.Dict.entries(jsContext)
  ->Belt.Array.reduce(Context.empty, (accum, (key, value)) =>
      Context.add(key, Result.unwrap(value), accum)
    );

let resolve = a => Result.wrap(eval(a));
let resolveWithContext = (jsContext, a) =>
  Result.wrap(eval(~context=_convertContext(jsContext), a));

let ofComplexFloats = (re, im) =>
  ofT(SciLineValue.ofNumber(Complex.ofFloats(re, im)));

let toFloat = a =>
  switch (SciLineValue.toNumber(Result.unwrap(a))) {
  | Some(comp) => Complex.toFloat(comp)
  | None => nan
  };

let toComplexFloats = a =>
  switch (SciLineValue.toNumber(Result.unwrap(a))) {
  | Some(comp) => Complex.toFloats(comp)
  | None => (nan, nan)
  };

let _mapMatrix = (fn, a) =>
  switch (SciLineValue.toMatrix(Result.unwrap(a))) {
  | Some(mat) =>
    NumberMatrix.toMatrix(mat)
    ->Belt.Array.map(row => Belt.Array.map(row, fn))
  | None => [||]
  };

let toFloatsMatrix = _mapMatrix(Complex.toFloat);
let toComplexFloatsMatrix = _mapMatrix(Complex.toFloats);

let isNan = a => SciLineValue.isNan(Result.unwrap(a));

let _f = a => SciLineValue.toNumber(Result.unwrap(a));
let _t = a => Result.wrap(SciLineValue.ofScalar(a));
let _nan = Result.wrap(SciLineValue.nan);
let solveQuadratic = (a, b, c) =>
  switch (_f(a), _f(b), _f(c)) {
  | (Some(a), Some(b), Some(c)) =>
    let (x0, x1) = Equation.quadratic(a, b, c);
    (_t(x0), _t(x1));
  | _ => (_nan, _nan)
  };
let solveCubic = (a, b, c, d) =>
  switch (_f(a), _f(b), _f(c), _f(d)) {
  | (Some(a), Some(b), Some(c), Some(d)) =>
    let (x0, x1, x2) = Equation.cubic(a, b, c, d);
    (_t(x0), _t(x1), _t(x2));
  | _ => (_nan, _nan, _nan)
  };
let solveVar2 = (x0, y0, c0, x1, y1, c1) =>
  switch (_f(x0), _f(y0), _f(c0), _f(x1), _f(y1), _f(c1)) {
  | (Some(x0), Some(y0), Some(c0), Some(x1), Some(y1), Some(c1)) =>
    let (x, y) = Equation.var2(x0, y0, c0, x1, y1, c1);
    (_t(x), _t(y));
  | _ => (_nan, _nan)
  };
let solveVar3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) =>
  switch (
    _f(x0),
    _f(y0),
    _f(z0),
    _f(c0),
    _f(x1),
    _f(y1),
    _f(z1),
    _f(c1),
    _f(x2),
    _f(y2),
    _f(z2),
    _f(c2),
  ) {
  | (
      Some(x0),
      Some(y0),
      Some(z0),
      Some(c0),
      Some(x1),
      Some(y1),
      Some(z1),
      Some(c1),
      Some(x2),
      Some(y2),
      Some(z2),
      Some(c2),
    ) =>
    let (x, y, z) =
      Equation.var3(x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2);
    (_t(x), _t(y), _t(z));
  | _ => (_nan, _nan, _nan)
  };

[@bs.deriving abstract]
type format = {
  [@bs.optional]
  mode: string,
  [@bs.optional]
  style: string,
  [@bs.optional]
  precision: int,
  [@bs.optional]
  base: int,
  [@bs.optional]
  decimalMinMagnitude: float,
  [@bs.optional]
  decimalMaxMagnitude: float,
};

let _format_with_mode = (mode, x, maybeFormat) => {
  let f = maybeFormat->Belt.Option.getWithDefault(format());
  let format = {
    OutputFormat.mode,
    style:
      switch (styleGet(f)) {
      | Some("decimal") => Decimal
      | Some("scientific") => Scientific
      | _ => Natural
      },
    precision:
      precisionGet(f)
      ->Belt.Option.getWithDefault(OutputFormat.default.precision),
    base: baseGet(f)->Belt.Option.getWithDefault(OutputFormat.default.base),
    decimalMinMagnitude:
      decimalMinMagnitudeGet(f)
      ->Belt.Option.getWithDefault(OutputFormat.default.decimalMinMagnitude),
    decimalMaxMagnitude:
      decimalMaxMagnitudeGet(f)
      ->Belt.Option.getWithDefault(OutputFormat.default.decimalMaxMagnitude),
  };
  SciLineValue.toString(~format, Result.unwrap(x));
};

let toString = (x, maybeFormat) => {
  open OutputFormat;
  let f = maybeFormat->Belt.Option.getWithDefault(format());

  let (mode, inline) =
    switch (modeGet(f)) {
    | Some("tex") => (Tex, false)
    | Some("mathml") => (MathML, false)
    | Some("mathml-inline") => (MathML, true)
    | _ => (String, false)
    };

  let format = {
    mode,
    style:
      switch (styleGet(f)) {
      | Some("decimal") => Decimal
      | Some("scientific") => Scientific
      | _ => Natural
      },
    precision:
      precisionGet(f)
      ->Belt.Option.getWithDefault(OutputFormat.default.precision),
    base: baseGet(f)->Belt.Option.getWithDefault(OutputFormat.default.base),
    decimalMinMagnitude:
      decimalMinMagnitudeGet(f)
      ->Belt.Option.getWithDefault(OutputFormat.default.decimalMinMagnitude),
    decimalMaxMagnitude:
      decimalMaxMagnitudeGet(f)
      ->Belt.Option.getWithDefault(OutputFormat.default.decimalMaxMagnitude),
  };

  let body = SciLineValue.toString(~format, Result.unwrap(x));

  switch (mode) {
  | String
  | Tex => body
  | MathML =>
    let display = inline ? "inline" : "block";
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\" display=\""
    ++ display
    ++ "\">"
    ++ body
    ++ "</math>";
  };
};

let encode = x => Result.unwrap(x)->SciLineValue.encode;
let decode = x => SciLineValue.decode(x)->Result.wrap;
