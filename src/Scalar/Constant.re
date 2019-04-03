type t =
  | None
  | Pi
  | Exp(int)
  | Sqrt(Z.t);

let none = None;

let to_float = a =>
  switch (a) {
  | None => 1.0
  | Pi => FloatUtil.pi
  | Exp(v) => exp(float_of_int(v))
  | Sqrt(v) => sqrt(Z.to_float(v))
  };

let to_q = a =>
  switch (a) {
  | None => Q.one
  | Pi => Q.of_float(FloatUtil.pi)
  | Exp(v) => Q.of_float(exp(float_of_int(v)))
  | Sqrt(v) =>
    let factor_mag = Z.log2(v) / 2 * 2;
    let factor = Z.pow(Z.of_int(2), factor_mag);
    let float_safe_base = Q.make(v, factor);
    let float_sqrt = float_safe_base |> Q.to_float |> sqrt |> Q.of_float;
    let factor_sqrt = QUtil.exp_ints(2, factor_mag / 2);
    Q.mul(float_sqrt, factor_sqrt);
  };

let equal = (a, b) =>
  switch (a, b) {
  | (None, None)
  | (Pi, Pi) => true
  | (Sqrt(ac), Sqrt(bc)) => Z.equal(ac, bc)
  | (Exp(ac), Exp(bc)) => ac == bc
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
  | Exp(0) => (Z.one, None)
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
  | (String, Exp(v)) => "exp(" ++ _number_string(format, Z.of_int(v)) ++ ")"
  | (String, Sqrt(v)) => "sqrt(" ++ _number_string(format, v) ++ ")"
  | (Latex, Pi) => "\\pi"
  | (Latex, Exp(1)) => "e"
  | (Latex, Exp(v)) => "e^{" ++ _number_string(format, Z.of_int(v)) ++ "}"
  | (Latex, Sqrt(v)) => "\\sqrt{" ++ _number_string(format, v) ++ "}"
  | (MathML, Pi) => "<mi>&pi;</mi>"
  | (MathML, Exp(1)) => "<mi>e</mi>"
  | (MathML, Exp(v)) =>
    "<msup><mi>e</mi><mn>"
    ++ _number_string(format, Z.of_int(v))
    ++ "</mn></msup>"
  | (MathML, Sqrt(v)) => "<msqrt>" ++ _number_string(format, v) ++ "</msqrt>"
  };
