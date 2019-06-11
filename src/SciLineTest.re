include AST;

let encode = Value.encode;
let decode = Value.decode;

let resolve = a => eval(a);
let resolveWithContext = (jsContext, a) => {
  let context =
    Js.Dict.entries(jsContext)
    ->Belt.Array.reduce(Belt.Map.String.empty, (accum, (key, value)) =>
        Belt.Map.String.set(accum, key, value)
      );
  eval(~context, a);
};

let valueOfString = Types.ofString;
let valueOfStringBase = Types.ofStringBase;
let toFloat = Types.toFloat;
let isNan = (a: Types.value) => a == `NaN;

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

let toString = (x, maybeFormat) => {
  open Formatting_Types;
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
      ->Belt.Option.getWithDefault(Formatting_Types.default.precision),
    base:
      baseGet(f)->Belt.Option.getWithDefault(Formatting_Types.default.base),
    decimalMinMagnitude:
      decimalMinMagnitudeGet(f)
      ->Belt.Option.getWithDefault(
          Formatting_Types.default.decimalMinMagnitude,
        ),
    decimalMaxMagnitude:
      decimalMaxMagnitudeGet(f)
      ->Belt.Option.getWithDefault(
          Formatting_Types.default.decimalMaxMagnitude,
        ),
  };

  Value.toString(~format, ~inline, x);
};

let ofComplexFloats = (re, im) =>
  Value.add(Types.ofFloat(re), Value.mul(Types.ofFloat(im), Types.i));

let toComplexFloats = (a): (float, float) =>
  switch (a) {
  | `Zero => (0., 0.)
  | `Real(q, c) => (QCUtil.toFloat(q, c), 0.)
  | `Imag(q, c) => (0., QCUtil.toFloat(q, c))
  | `Complex(reQ, reC, imQ, imC) => (
      QCUtil.toFloat(reQ, reC),
      QCUtil.toFloat(imQ, imC),
    )
  | _ => Pervasives.(nan, nan)
  };

let _mapMatrix = (fn: Types.value => 'a, a: Types.value): array(array('a)) => {
  let fn = x => Types.valueOfScalar(x)->fn;

  switch (a) {
  | `Vector([|a, b|]) => [|[|fn(a)|], [|fn(b)|]|]
  | `Vector([|a, b, c|]) => [|[|fn(a)|], [|fn(b)|], [|fn(c)|]|]
  | `Matrix({elements: [|a, b, c, d|]}) => [|
      [|fn(a), fn(b)|],
      [|fn(c), fn(d)|],
    |]
  | `Matrix({elements: [|a, b, c, d, e, f, g, h, i|]}) => [|
      [|fn(a), fn(b), fn(c)|],
      [|fn(d), fn(e), fn(f)|],
      [|fn(g), fn(h), fn(i)|],
    |]
  | _ => invalid_arg("Not a matrix")
  };
};

let toFloatsMatrix = _mapMatrix(Types.toFloat);
let toComplexFloatsMatrix = _mapMatrix(toComplexFloats);
