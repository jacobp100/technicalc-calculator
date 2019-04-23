/* Accurate to 50dp */
let e = Q.of_string("47044938235620704501191752/17306865588065164490357443");
let pi = Q.of_string("23294267674065827396789607/7414795692066647773964845");

let pow = (base, a) =>
  if (a == 0) {
    Q.(base != zero) ? Q.one : Q.undef;
  } else if (a > 0) {
    Q.make(Q.num(base)->Z.pow(a), Q.den(base)->Z.pow(a));
  } else {
    let a = abs(a);
    Q.make(Q.den(base)->Z.pow(a), Q.num(base)->Z.pow(a));
  };

let powInt = (base, a) =>
  if (a == 0) {
    base != 0 ? Q.one : Q.undef;
  } else if (a > 0) {
    Q.of_bigint(Z.pow(Z.of_int(base), a));
  } else {
    Q.of_bigint(Z.pow(Z.of_int(base), abs(a)))->Q.inv;
  };

let sqrtZ = a => {
  let factor_mag = Z.log2(a) / 2 * 2;
  let factor = Z.pow(Z.of_int(2), factor_mag);
  let float_safe_base = Q.make(a, factor);
  let float_sqrt = float_safe_base->Q.to_float->sqrt->Q.of_float;
  let factor_sqrt = powInt(2, factor_mag / 2);
  Q.mul(float_sqrt, factor_sqrt);
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
      if (Q.geq(absX, powInt(base, mid))) {
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

let isInt = a => Z.(Q.den(a) == one);

let floor = a =>
  if (isInt(a)) {
    Q.num(a);
  } else {
    Z.div(Q.num(a), Q.den(a));
  };
