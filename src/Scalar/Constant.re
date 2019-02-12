type t =
  | None
  | Pi
  | Exp(Z.t)
  | Sqrt(Z.t);

let none = None;

let to_float = a =>
  switch (a) {
  | None => 1.0
  | Pi => FloatUtil.pi
  | Exp(v) => exp(Z.to_float(v))
  | Sqrt(v) => sqrt(Z.to_float(v))
  };

let equal = (a, b) =>
  switch (a, b) {
  | (None, None)
  | (Pi, Pi) => true
  | (Sqrt(ac), Sqrt(bc))
  | (Exp(ac), Exp(bc)) => Z.equal(ac, bc)
  | _ => false
  };

let simplify_sqrt = ac =>
  if (Z.equal(ac, Z.zero)) {
    (Z.zero, None);
  } else if (Z.equal(ac, Z.one)) {
    (Z.one, None);
  } else {
    let upper = ac |> Z.to_float |> sqrt |> Z.of_float;

    let sqrt_arg = ref(ac);
    let multiplier = ref(Z.one);

    let current_sqrt_value = ref(Z.of_int(2));
    while (Z.leq(current_sqrt_value^, upper)) {
      let factor = Z.mul(current_sqrt_value^, current_sqrt_value^);

      while (Z.equal(Z.rem(sqrt_arg^, factor), Z.zero)) {
        sqrt_arg := Z.div(sqrt_arg^, factor);
        multiplier := Z.mul(multiplier^, current_sqrt_value^);
      };

      current_sqrt_value := Z.add(current_sqrt_value^, Z.one);
    };

    let constant = Z.equal(sqrt_arg^, Z.one) ? None : Sqrt(sqrt_arg^);
    (multiplier^, constant);
  };

let simplify = a =>
  switch (a) {
  | Sqrt(ac) => simplify_sqrt(ac)
  | Exp(ac) when Z.equal(ac, Z.zero) => (Z.one, None)
  | v => (Z.one, v)
  };

let _number_string = (format, v) =>
  NumberFormat.format_integer(
    ~base=OutputFormat.(format.base),
    NumberFormat.create_format(~digit_separators=false, ()),
    v,
  );

let to_string = (~format=OutputFormat.default, a) =>
  switch (format.mode, a) {
  | (_, None) => ""
  | (String, Pi) => "pi"
  | (String, Exp(v)) => "exp(" ++ _number_string(format, v) ++ ")"
  | (String, Sqrt(v)) => "sqrt(" ++ _number_string(format, v) ++ ")"
  | (Latex, Pi) => "\\pi"
  | (Latex, Exp(v)) when Z.equal(v, Z.one) => "e"
  | (Latex, Exp(v)) => "e^{" ++ _number_string(format, v) ++ "}"
  | (Latex, Sqrt(v)) => "\\sqrt{" ++ _number_string(format, v) ++ "}"
  };
