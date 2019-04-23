include AST;

let _convertContext = jsContext =>
  Js.Dict.entries(jsContext)
  ->Belt.Array.reduce(Context.empty, (accum, (key, value)) =>
      Context.add(key, value, accum)
    );

let resolve = a => eval(a);
let resolveWithContext = (jsContext, a) =>
  eval(~context=_convertContext(jsContext), a);

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
  Value.toString(~format, x);
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

  let body = Value.toString(~format, x);

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
