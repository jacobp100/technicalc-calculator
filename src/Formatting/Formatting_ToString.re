open Types;
open Formatting_Types;

let formatNumber = (x, format) =>
  switch (format.mode) {
  | String
  | Tex => x
  | MathML => "<mn>" ++ x ++ "</mn>"
  };

let formatExponential = ((base, exponent), format) =>
  switch (format.mode) {
  | String => base ++ "e" ++ exponent
  | Tex => base ++ "*10^{" ++ exponent ++ "}"
  | MathML =>
    formatNumber(base, format)
    ++ "<mo>&times;</mo><msup><mn>10</mn>"
    ++ formatNumber(exponent, format)
    ++ "</msup>"
  };

let formatOperator = (op, format) =>
  format.mode == MathML ? "<mo>" ++ op ++ "</mo>" : op;

let formatVariable = (var, format) =>
  format.mode == MathML ? "<mi>" ++ var ++ "</mi>" : var;

let formatTuple = (re, format): string => {
  let base = format.base;

  switch (re, format.style) {
  | (Real.Rational(n, d, c), Natural) =>
    let formatting =
      Formatting_Number.createFormat(~digitSeparators=true, ());
    let minus = n < 0 ? formatOperator("-", format) : "";

    let (top, needsWrap) =
      switch (
        Formatting_Number.formatInteger(
          ~base,
          formatting,
          abs(d)->float_of_int,
        ),
        Formatting_Constant.toString(~format, c),
      ) {
      | ("1", "") => (formatNumber("1", format), false)
      | ("1", constant) => (constant, false)
      | (numerator, constant) => (
          formatNumber(numerator, format) ++ constant,
          true,
        )
      };

    switch (
      format.mode,
      Formatting_Number.formatInteger(~base, formatting, float_of_int(d)),
    ) {
    | (_, "1") => minus ++ top
    | (String, bottom) => minus ++ top ++ "/" ++ bottom
    | (Tex, bottom) => minus ++ "\\frac{" ++ top ++ "}{" ++ bottom ++ "}"
    | (MathML, denominator) =>
      let top = needsWrap ? "<mrow>" ++ top ++ "</mrow>" : top;
      let bottom = formatNumber(denominator, format);
      minus ++ "<mfrac>" ++ top ++ bottom ++ "</mfrac>";
    };
  | (_, Natural | Decimal) =>
    let floatVal = Real.toFloat(re);
    let valueMagnitude = floor(log10(abs_float(floatVal)));
    let insideMagnitudeThreshold =
      valueMagnitude >= format.decimalMinMagnitude
      && valueMagnitude <= format.decimalMaxMagnitude;

    if (insideMagnitudeThreshold) {
      Formatting_Number.formatDecimal(
        ~base,
        Formatting_Number.createFormat(
          ~maxDecimalPlaces=format.precision,
          ~digitSeparators=valueMagnitude >= 5.,
          (),
        ),
        floatVal,
      )
      ->formatNumber(format);
    } else {
      Formatting_Number.formatExponential(
        ~base,
        ~exponent=DecimalUtil.magnitude(floatVal),
        Formatting_Number.createFormat(
          ~maxDecimalPlaces=format.precision,
          (),
        ),
        floatVal,
      )
      ->formatExponential(format);
    };
  | (_, Scientific) =>
    /* Round to multiple of 3 */
    let floatVal = Real.toFloat(re);
    let exponent = DecimalUtil.magnitude(floatVal) * 3 / 3;
    let formatting =
      Formatting_Number.createFormat(
        ~minDecimalPlaces=format.precision,
        ~maxDecimalPlaces=format.precision,
        (),
      );
    Formatting_Number.formatExponential(
      ~base,
      ~exponent,
      formatting,
      floatVal,
    )
    ->formatExponential(format);
  };
};

let formatImagTuple = (re: Real.t, format): string => {
  let i = formatVariable("i", format);
  switch (format.style, re) {
  | (Natural | Decimal, Rational(1, 1, Unit)) => i
  | (Natural | Decimal, Rational((-1), 1, Unit)) =>
    formatOperator("-", format) ++ i
  | _ => formatTuple(re, format) ++ i
  };
};

let formatScalar = (a: scalar, format): string =>
  switch (a) {
  | `Zero => formatNumber("0", format)
  | `Real(re) => formatTuple(re, format)
  | `Imag(im) => formatImagTuple(im, format)
  | `Complex(re, im) =>
    formatTuple(re, format)
    ++ formatOperator(Real.toFloat(im) < 0. ? "-" : "+", format)
    ++ formatImagTuple(Real.abs(im), format)
  };

let toString = (~format=default, ~inline=false, a: value): string => {
  let body =
    switch (a) {
    | (`Zero | `Real(_) | `Imag(_) | `Complex(_)) as aV =>
      formatScalar(aV, format)
    | (`Matrix(_) | `Vector(_)) as aV =>
      let m =
        switch (aV) {
        | `Matrix(m) => m
        | `Vector(elements) => Matrix.ofVector(elements)
        };
      open Formatting_Matrix;
      let fmt =
        switch (format.mode) {
        | String => matrixFormatString
        | Tex => matrixFormatTex
        | MathML => matrixFormatMathML
        };
      formatMatrix(Matrix.map(m, s => formatScalar(s, format)), fmt);
    | `Percent(p) => formatScalar(p, format) ++ formatOperator("%", format)
    | `NaN => formatVariable("NaN", format)
    };

  switch (format.mode) {
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