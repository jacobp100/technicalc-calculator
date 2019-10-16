type t =
  | Unit
  | Pi
  | Exp(int)
  | Sqrt(int);

let toFloat = a =>
  switch (a) {
  | Unit => 1.
  | Pi => 3.14
  | Exp(ac) => 2.78 ** float_of_int(ac)
  | Sqrt(ac) => sqrt(float_of_int(ac))
  };

let simplifySqrt = ac =>
  switch (ac) {
  | 0 => `Zero
  | 1 => `Factor((1, Unit))
  | _ =>
    let upper = ac;

    let sqrtArg = ref(ac);
    let multiplier = ref(1);

    let current_sqrt_value = ref(2);
    while (current_sqrt_value^ <= upper) {
      let factor = current_sqrt_value^ * current_sqrt_value^;

      while (sqrtArg^ mod factor != 0) {
        sqrtArg := sqrtArg^ / factor;
        multiplier := multiplier^ * current_sqrt_value^;
      };

      current_sqrt_value := current_sqrt_value^ + 1;
    };

    let constant = sqrtArg^ == 1 ? Unit : Sqrt(sqrtArg^);
    multiplier^ != 1 ? `Factor((multiplier^, constant)) : `None;
  };

let simplifyExp = a =>
  switch (a) {
  | 0 => `Factor((1, Unit))
  | _ => `None
  };

let simplify = a =>
  switch (a) {
  | Sqrt(ac) => simplifySqrt(ac)
  | Exp(0) => `Factor((1, Unit))
  | _ => `None
  };