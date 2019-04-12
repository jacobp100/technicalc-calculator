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

let _encodeValue: SciLineValue.t => string = [%raw
  {|
    x => {
      const BN = require("bn.js");
      const isRat = require("big-rat/is-rat");

      const transform = (key, value) => {
        if (Array.isArray(value) && value.tag != null) {
          return {
            ...value.map((value, key) => transform(key, value)),
            tag: value.tag,
            length: value.length
          };
        } else if (isRat(value)) {
          return `${value[0]}/${value[1]}`;
        } else if (BN.isBN(value)) {
          return value.toString();
        }
        return value;
      };

      return JSON.stringify(x, transform);
    }
  |}
];

let encode = x => _encodeValue(Result.unwrap(x));

let _decodeValue: string => SciLineValue.t = [%raw
  {|
    x => {
      const BN = require("bn.js");
      const rat = require("big-rat");

      const transform = (key, value) => {
        if (value.tag != null) {
          const arr = Array.from(value);
          arr.tag = value.tag;
          return arr;
        } else if (typeof value === "string" && /^-?\d+\/\d+$/.test(value)) {
          const [num, den] = value.split("/");
          return rat(num, den);
        } else if (typeof value === "string" && /^-?\d+$/.test(value)) {
          return new BN(value);
        }
        return value;
      };

      return JSON.parse(x, transform);
    }
  |}
];

let decode = x => Result.wrap(_decodeValue(x));
