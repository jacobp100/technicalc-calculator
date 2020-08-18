open Formatting_Types;

let%private f = (format, v) =>
  Formatting_Number.formatInteger(
    ~base=format.base,
    ~digitGrouping=format.digitGrouping,
    Decimal.ofInt(v),
  );

let toString = (~format=default, a) =>
  switch (format.mode, a) {
  | (_, Real_Constant.Unit) => ""
  | (String, Pi) => "pi"
  | (String, Exp(v)) => "exp(" ++ f(format, v) ++ ")"
  | (String, Sqrt(v)) => "sqrt(" ++ f(format, v) ++ ")"
  | (Tex, Pi) => "\\pi"
  | (Tex, Exp(1)) => "e"
  | (Tex, Exp(v)) => "e^{" ++ f(format, v) ++ "}"
  | (Tex, Sqrt(v)) => "\\sqrt{" ++ f(format, v) ++ "}"
  | (MathML, Pi) => "<mi>&#960;</mi>"
  | (MathML, Exp(1)) => "<mi>e</mi>"
  | (MathML, Exp(v)) =>
    "<msup><mi>e</mi><mn>" ++ f(format, v) ++ "</mn></msup>"
  | (MathML, Sqrt(v)) => "<msqrt><mn>" ++ f(format, v) ++ "</mn></msqrt>"
  };
