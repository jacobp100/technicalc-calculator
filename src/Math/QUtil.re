let expInts = (base, a) =>
  if (a == 0) {
    Q.one;
  } else if (a > 0) {
    Q.of_bigint(Z.pow(Z.of_int(base), a));
  } else {
    Q.of_bigint(Z.pow(Z.of_int(base), abs(a)))->Q.inv;
  };

let magnitude = x =>
  if (Q.equal(x, Q.zero)) {
    0;
  } else {
    let base = 10; /* Easily refactored if needed */
    let absX = Q.abs(x);
    let (num, den) = (Q.num(absX), Q.den(absX));
    let baseLog2 = log(float_of_int(base)) /. log(2.);
    let lowerBound =
      (float_of_int(Z.log2(num) - Z.log2up(den)) /. baseLog2)
      ->floor
      ->int_of_float
      ->ref;
    let upperBound =
      (float_of_int(Z.log2up(num) - Z.log2(den)) /. baseLog2)
      ->ceil
      ->int_of_float
      ->ref;

    while (upperBound^ - lowerBound^ > 1) {
      let mid = (lowerBound^ + upperBound^) / 2;
      if (Q.geq(absX, expInts(base, mid))) {
        lowerBound := mid;
      } else {
        upperBound := mid;
      };
    };

    lowerBound^;
  };

let safeMod = (a, b) => {
  let denominator = Q.den(a);
  let divisor = Z.mul(b, denominator);
  let numerator = Z.rem(Q.num(a), divisor);
  let numerator =
    switch (Z.sign(numerator)) {
    | 0
    | 1 => numerator
    | (-1) => Z.rem(Z.add(numerator, divisor), divisor)
    | _ => raise(Not_found)
    };
  Q.make(numerator, denominator);
};

let isInt = a => Z.equal(Q.den(a), Z.one);

let floor = a =>
  if (isInt(a)) {
    Q.num(a);
  } else {
    Z.div(Q.num(a), Q.den(a));
  };
