open Formatting_Types;

let _f = (format, v) =>
  Formatting_Number.formatInteger(
    ~base=format.base,
    Formatting_Number.createFormat(~digitSeparators=false, ()),
    v,
  );

let toString = (~format=default, a) =>
  switch (format.mode, a) {
  | (_, Constant.Unit) => ""
  | (String, Pi) => "pi"
  | (String, Exp(v)) => "exp(" ++ _f(format, Z.of_int(v)) ++ ")"
  | (String, Sqrt(v)) => "sqrt(" ++ _f(format, v) ++ ")"
  | (Tex, Pi) => "\\pi"
  | (Tex, Exp(1)) => "e"
  | (Tex, Exp(v)) => "e^{" ++ _f(format, Z.of_int(v)) ++ "}"
  | (Tex, Sqrt(v)) => "\\sqrt{" ++ _f(format, v) ++ "}"
  | (MathML, Pi) => "<mi>&pi;</mi>"
  | (MathML, Exp(1)) => "<mi>e</mi>"
  | (MathML, Exp(v)) =>
    "<msup><mi>e</mi><mn>" ++ _f(format, Z.of_int(v)) ++ "</mn></msup>"
  | (MathML, Sqrt(v)) => "<msqrt><mn>" ++ _f(format, v) ++ "</mn></msqrt>"
  };
