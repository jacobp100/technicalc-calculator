open Types;

let qTwo = Q.of_int(2);
let qHalf = Q.of_ints(1, 2);
let half = `Real((qHalf, Constant.Unit));

let rec pow = (a: value, b: value): value =>
  switch (a, b) {
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_)) => `Zero
  | (`Real(_) | `Imag(_) | `Complex(_), `Zero) => one
  | (`Zero, `Zero) => `NaN
  | (`Real(aReQ, aReC), `Real(isHalf, Constant.Unit))
      when Q.(isHalf == qHalf) =>
    let q = Q.abs(aReQ);
    let denSqrt = Q.den(q)->QUtil.sqrtZ;
    let (q, c) =
      if (QUtil.isInt(denSqrt) && Constant.(aReC == Unit)) {
        (Q.inv(denSqrt), Constant.Sqrt(Q.num(q)));
      } else {
        (QCUtil.mapFloat(q, aReC, sqrt), Unit);
      };
    if (Q.(aReQ > zero)) {
      realQC(q, c);
    } else {
      imagQC(q, c);
    };
  | (`Real(isOne, Constant.Exp(1)), _) when Q.(isOne == one) => Exp.exp(b)
  | (_, `Real(isTwo, Unit)) when Q.(isTwo == qTwo) => BasicMath.(a * a)
  | (`Real(aReQ, Unit), `Real(isInt, Unit)) when QUtil.isInt(isInt) =>
    switch (Q.num(isInt)->Z.to_int) {
    | bn => real(QUtil.pow(aReQ, bn))
    | exception Z.Overflow => `NaN
    }
  | (`Real(aReQ, Unit), `Real(ltZero, Unit)) when Q.(ltZero < zero) =>
    pow(real(Q.inv(aReQ)), real(Q.abs(ltZero)))
  | (`Imag(aImQ, aImC), `Real(isInt, Unit)) when QUtil.isInt(isInt) =>
    let aPowB = pow(realQC(aImQ, aImC), b);
    switch (Q.num(isInt)->ZUtil.safeMod(Z.of_int(4))->Z.to_int) {
    | 0 => aPowB
    | 1 => BasicMath.(aPowB * i)
    | 2 => BasicMath.(- aPowB)
    | 3 => BasicMath.(aPowB * minusI)
    | _ => raise(Not_found)
    };
  | (`Real(_) | `Imag(_) | `Complex(_), `Real(_) | `Imag(_) | `Complex(_)) =>
    BasicMath.(Exp.exp(Exp.log(a) * b))
  | (`Matrix2(a, b, c, d), `Real(isMinusOne, Unit))
      when Q.(isMinusOne == minus_one) =>
    let (~-) = BasicMath.negScalar;
    let (-) = BasicMath.subScalar;
    let ( * ) = BasicMath.mulScalar;
    let (/) = BasicMath.divScalar;
    let factor = a * d - b * c;
    matrix2(d / factor, - b / factor, - c / factor, a / factor);
  | (`Matrix3(a, b, c, d, e, f, g, h, i), `Real(isMinusOne, Unit))
      when Q.(isMinusOne == minus_one) =>
    /* https://www.wolframalpha.com/input/?i=%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D%5E-1 */
    let (+) = BasicMath.addScalar;
    let (-) = BasicMath.subScalar;
    let ( * ) = BasicMath.mulScalar;
    let (/) = BasicMath.divScalar;
    let factor =
      a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g;
    matrix3(
      (e * i - f * h) / factor,
      (c * h - b * i) / factor,
      (b * f - c * e) / factor,
      (f * g - d * i) / factor,
      (a * i - c * g) / factor,
      (c * d - a * f) / factor,
      (d * h - e * g) / factor,
      (b * g - a * h) / factor,
      (a * e - b * d) / factor,
    );
  | ((`Matrix2(_) | `Matrix3(_)) as aM, `Zero) => MatrixUtil.toIdentity(aM)
  | ((`Matrix2(_) | `Matrix3(_)) as aM, `Real(intGtZero, Unit))
      when Q.(intGtZero >= zero) && QUtil.isInt(intGtZero) =>
    let x = ref(MatrixUtil.toIdentity(aM));
    for (_ in 0 to Q.num(intGtZero)->Z.to_int - 1) {
      x := BasicMath.(x^ * a);
    };
    x^;
  | (`NaN | `Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_), _) => `NaN
  | (_, `NaN | `Vector2(_) | `Vector3(_) | `Matrix2(_) | `Matrix3(_)) => `NaN
  };

let sqrt = (x: value): value => pow(x, half);
