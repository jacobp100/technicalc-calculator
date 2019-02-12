let exp_ints = (base, a) =>
  if (a == 0) {
    Q.one;
  } else if (a > 0) {
    Q.of_bigint(Z.pow(Z.of_int(base), a));
  } else {
    Q.of_bigint(Z.pow(Z.of_int(base), abs(a))) |> Q.inv;
  };

let magnitude = x =>
  if (Q.equal(x, Q.zero)) {
    0;
  } else {
    let base = 10; /* Easily refactored if needed */
    let abs_x = Q.abs(x);
    let (num, den) = (Q.num(abs_x), Q.den(abs_x));
    let base_log2 = log(float_of_int(base)) /. log(2.);
    let lower_bound =
      float_of_int(Z.log2(num) - Z.log2up(den))
      /. base_log2
      |> floor
      |> int_of_float
      |> ref;
    let upper_bound =
      float_of_int(Z.log2up(num) - Z.log2(den))
      /. base_log2
      |> ceil
      |> int_of_float
      |> ref;

    while (upper_bound^ - lower_bound^ > 1) {
      let mid = (lower_bound^ + upper_bound^) / 2;
      if (Q.geq(abs_x, exp_ints(base, mid))) {
        lower_bound := mid;
      } else {
        upper_bound := mid;
      };
    };

    lower_bound^;
  };

let safe_mod_z = (a, b) => {
  let denominator = Q.den(a);
  let divisor = Z.mul(b, denominator);
  let numerator = Z.rem(Q.num(a), divisor);
  /* Handle negative numerators */
  let numerator = Z.rem(Z.add(numerator, divisor), divisor);
  Q.make(numerator, denominator);
};

let is_int = a => Z.equal(Q.den(a), Z.one);

let floor = a =>
  Q.num(
    if (is_int(a)) {
      a;
    } else {
      let integer_part = safe_mod_z(a, Z.one);
      Q.sub(a, integer_part);
    },
  );
