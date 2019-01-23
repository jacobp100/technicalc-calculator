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

let simplify_sqrt = ac =>
  if (Zt.equal(ac, Zt.zero)) {
    (Zt.zero, None);
  } else if (Zt.equal(ac, Zt.one)) {
    (Zt.one, None);
  } else {
    let upper = ac |> Zt.to_float |> sqrt |> Zt.of_float;

    let sqrt_arg = ref(ac);
    let multiplier = ref(Zt.one);

    let current_sqrt_value = ref(Zt.of_int(2));
    while (Zt.lte(current_sqrt_value^, upper)) {
      let factor = Zt.mul(current_sqrt_value^, current_sqrt_value^);

      while (Zt.equal(Zt.rem(sqrt_arg^, factor), Zt.zero)) {
        sqrt_arg := Zt.div(sqrt_arg^, factor);
        multiplier := Zt.mul(multiplier^, current_sqrt_value^);
      };

      current_sqrt_value := Zt.add(current_sqrt_value^, Zt.one);
    };

    let constant = Zt.equal(sqrt_arg^, Zt.one) ? None : Sqrt(sqrt_arg^);
    (multiplier^, constant);
  };

let simplify = a =>
  switch (a) {
  | Sqrt(ac) => simplify_sqrt(ac)
  | Exp(ac) when Zt.equal(ac, Zt.zero) => (Zt.one, None)
  | v => (Zt.one, v)
  };

let to_string = (~format=OutputFormat.default, a) =>
  switch (format.mode, a) {
  | (_, None) => ""
  | (String, Pi) => "pi"
  | (String, Exp(v)) =>
    "exp("
    ++ NumberFormat.format_integer(
         NumberFormat.create_format(~digit_separators=false, ()),
         v,
       )
    ++ ")"
  | (String, Sqrt(v)) =>
    "sqrt("
    ++ NumberFormat.format_integer(
         NumberFormat.create_format(~digit_separators=false, ()),
         v,
       )
    ++ ")"
  | (Latex, Pi) => "\\pi"
  | (Latex, Exp(v)) when Zt.equal(v, Zt.one) => "e"
  | (Latex, Exp(v)) =>
    "e^{"
    ++ NumberFormat.format_integer(
         NumberFormat.create_format(~digit_separators=false, ()),
         v,
       )
    ++ "}"
  | (Latex, Sqrt(v)) =>
    "\\sqrt{"
    ++ NumberFormat.(
         format_integer(
           NumberFormat.create_format(~digit_separators=true, ()),
           v,
         )
       )
    ++ "}"
  };
