type t =
  | Unit
  | Pi
  | Exp(int)
  | Sqrt(Z.t);

let toQ = a =>
  switch (a) {
  | Unit => Q.one
  | Pi => QUtil.pi
  | Exp(v) => QUtil.pow(QUtil.e, v)
  | Sqrt(v) => QUtil.sqrtZ(v)
  };

let equal = (a, b) =>
  switch (a, b) {
  | (Unit, Unit)
  | (Pi, Pi) => true
  | (Sqrt(ac), Sqrt(bc)) => Z.equal(ac, bc)
  | (Exp(ac), Exp(bc)) => ac == bc
  | _ => false
  };

let simplifySqrt = ac =>
  if (Z.equal(ac, Z.zero)) {
    `Zero;
  } else if (Z.equal(ac, Z.one)) {
    `Factor((Q.one, Unit));
  } else {
    let upper = ac->Z.to_float->sqrt->Z.of_float;

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

    let constant = Z.equal(sqrt_arg^, Z.one) ? Unit : Sqrt(sqrt_arg^);
    Z.(multiplier^ != one) ?
      `Factor((Q.of_bigint(multiplier^), constant)) : `None;
  };

let simplify = a =>
  switch (a) {
  | Sqrt(ac) => simplifySqrt(ac)
  | Exp(0) => `Factor((Q.one, Unit))
  | _ => `None
  };

let _f = (format, v) =>
  NumberFormat.formatInteger(
    ~base=OutputFormat.(format.base),
    NumberFormat.createFormat(~digitSeparators=false, ()),
    v,
  );

let toString = (~format=OutputFormat.default, a) =>
  switch (format.mode, a) {
  | (_, Unit) => ""
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

let (==) = equal;
