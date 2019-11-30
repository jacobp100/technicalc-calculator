open Formatting_Types;

let _f = (format, v) =>
  Formatting_Number.formatInteger(
    ~base=format.base,
    Formatting_Number.createFormat(~digitSeparators=false, ()),
    Decimal.ofInt(v),
  );

let toString = (~format=default, a) =>
  switch (format.mode, a) {
  | (_, Real_Constant.Unit) => ""
  | (String, Pi) => "pi"
  | (String, Exp(v)) => "exp(" ++ _f(format, v) ++ ")"
  | (String, Sqrt(v)) => "sqrt(" ++ _f(format, v) ++ ")"
  | (Tex, Pi) => "\\pi"
  | (Tex, Exp(1)) => "e"
  | (Tex, Exp(v)) => "e^{" ++ _f(format, v) ++ "}"
  | (Tex, Sqrt(v)) => "\\sqrt{" ++ _f(format, v) ++ "}"
  | (MathML, Pi) => "<mi>&#960;</mi>"
  | (MathML, Exp(1)) => "<mi>e</mi>"
  | (MathML, Exp(v)) =>
    "<msup><mi>e</mi><mn>" ++ _f(format, v) ++ "</mn></msup>"
  | (MathML, Sqrt(v)) => "<msqrt><mn>" ++ _f(format, v) ++ "</mn></msqrt>"
  };