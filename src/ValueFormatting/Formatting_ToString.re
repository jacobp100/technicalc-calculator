open Formatting_Types;
open Formatting_Util;

let toString = (~format=default, ~inline=false, a: Value_Types.t): string => {
  let body =
    switch (a) {
    | (`Z | `R(_) | `I(_) | `C(_)) as aV =>
      Formatting_Scalar.toString(aV, format)
    | (`M(_) | `V(_)) as aV =>
      let matrix =
        switch (aV) {
        | `M(m) => m
        | `V(v) => Matrix.ofVector(v)
        };
      let tableFormat =
        switch (format.mode) {
        | String => Formatting_Matrix.formatString
        | Tex => Formatting_Matrix.formatTex
        | MathML => Formatting_Matrix.formatMathML
        };
      Formatting_Matrix.toString(matrix, format, tableFormat);
    | `P(p) =>
      Formatting_Scalar.toString(p, format) ++ formatOperator("%", format)
    | `N => formatVariable("NaN", format)
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
