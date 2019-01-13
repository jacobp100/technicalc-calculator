module Zt = Z.Bigint;

type t =
  | None
  | Pi
  | Exp(Zt.t)
  | Sqrt(Zt.t);

let none = None;

let to_float = a =>
  switch (a) {
  | None => 1.0
  | Pi => 4.0 *. atan(1.0)
  | Exp(v) => exp(Zt.to_float(v))
  | Sqrt(v) => sqrt(Zt.to_float(v))
  };

let equal = (a, b) =>
  switch (a, b) {
  | (None, None)
  | (Pi, Pi) => true
  | (Sqrt(ac), Sqrt(bc))
  | (Exp(ac), Exp(bc)) => Zt.equal(ac, bc)
  | _ => false
  };

let to_string = a =>
  switch (a) {
  | None => ""
  | Pi => "pi"
  | Exp(v) => "exp(" ++ Zt.to_string(v) ++ ")"
  | Sqrt(v) => "sqrt(" ++ Zt.to_string(v) ++ ")"
  };

let to_latex = a =>
  switch (a) {
  | None => ""
  | Pi => "pi"
  | Exp(v) when Zt.equal(v, Zt.one) => "e"
  | Exp(v) => "e^{" ++ Zt.to_string(v) ++ "}"
  | Sqrt(v) => "\\sqrt{" ++ Zt.to_string(v) ++ "}"
  };
