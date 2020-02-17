open Types;

let (~-) = Base_Functions.neg;
let (+) = Base_Operators.add;
let (-) = Base_Operators.sub;
let ( * ) = Base_Operators.mul;
let (/) = Base_Operators.div;
let (==) = Base_Comparison.equal;
let exp = Base_Exponentiation.exp;
let log = Base_Exponentiation.log;

let halfS = `Real(Real.Rational(1, 2, Unit));
let zeroS = `Real(Real.zero);
let oneS = `Real(Real.one);
let toIdentity = m =>
  `Matrix(
    Matrix.mapWithIndex(m, (row, column, _) =>
      Pervasives.(row == column) ? oneS : zeroS
    ),
  );

let isSquare = x => float_of_int(x)->sqrt->FloatUtil.isInt;

let rec pow = (a: value, b: value): value =>
  switch (a, b) {
  | (`Zero, `Real(_) | `Imag(_) | `Complex(_)) => `Zero
  | (`Real(_) | `Imag(_) | `Complex(_), `Zero) => one
  | (`Zero, `Zero) => `NaN
  | (`Real(Rational(n, d, Unit)), `Real(Rational(1, 2, Unit)))
      when isSquare(d) =>
    switch (float_of_int(d)->sqrt->FloatUtil.intValue) {
    | Some(denSqrt) =>
      let r = Real.rational(1, denSqrt, Sqrt(abs(n)));
      if (n >= 0) {
        real(r);
      } else {
        imag(r);
      };
    | _ => failwith("Invalid rational provided to pow")
    }
  | (`Real(Rational(1, 1, Exp(1))), _) => exp(b)
  | (`Vector(_), `Real(Rational(2, 1, Unit))) => Base_Dot.dot(a, a)
  | (_, `Real(Rational(2, 1, Unit))) => a * a
  | (`Real(re), `Real(Rational(bInt, 1, Unit))) =>
    real(Real.powInt(re, bInt))
  | (`Imag(im), `Real(Rational(bInt, 1, Unit))) =>
    let aPowB = Real.powInt(im, bInt);
    switch (IntUtil.safeMod(bInt, 4)) {
    | 0 => real(aPowB)
    | 1 => imag(aPowB)
    | 2 => real(Real.(- aPowB))
    | 3 => imag(Real.(- aPowB))
    | _ => raise(Not_found)
    };
  | (`Real(_) | `Imag(_) | `Complex(_), `Real(_) | `Imag(_) | `Complex(_)) =>
    exp(log(a) * b)
  | (`Percent(aP), `Percent(bP)) =>
    pow(Base_Util.percentToNumerical(aP), Base_Util.percentToNumerical(bP))
  | (`Percent(p), _) => pow(Base_Util.percentToNumerical(p), b)
  | (_, `Percent(p)) => pow(a, Base_Util.percentToNumerical(p))
  | (
      `Matrix({numRows: 2, numColumns: 2, elements: [|a, b, c, d|]}),
      `Real(Rational((-1), 1, Unit)),
    ) =>
    let (~-) = Base_Functions.negScalar;
    let (-) = Base_Operators.subScalar;
    let ( * ) = Base_Operators.mulScalar;
    let (/) = Base_Operators.divScalar;
    let factor = a * d - b * c;
    matrix2(d / factor, - b / factor, - c / factor, a / factor);
  | (
      `Matrix({
        numRows: 3,
        numColumns: 3,
        elements: [|a, b, c, d, e, f, g, h, i|],
      }),
      `Real(Rational((-1), 1, Unit)),
    ) =>
    /* https://www.wolframalpha.com/input/?i=%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D%5E-1 */
    let (+) = Base_Operators.addScalar;
    let (-) = Base_Operators.subScalar;
    let ( * ) = Base_Operators.mulScalar;
    let (/) = Base_Operators.divScalar;
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
  | (
      `Matrix(
        ({numRows: 2, numColumns: 2} | {numRows: 3, numColumns: 3}) as aM,
      ),
      `Zero,
    ) =>
    toIdentity(aM)
  | (`Matrix(aM) as a, `Real(Rational(gtZero, 1, Unit))) when gtZero >= 0 =>
    let x = ref(toIdentity(aM));
    for (_ in 0 to Pervasives.(gtZero - 1)) {
      x := x^ * a;
    };
    x^;
  | (`NaN | `Vector(_) | `Matrix(_), _) => `NaN
  | (_, `NaN | `Vector(_) | `Matrix(_)) => `NaN
  };

let sqrt = (x: value): value => pow(x, halfS);