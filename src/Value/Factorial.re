open Types;

let g = Q.of_float(4.7421875);

let p0 = Q.of_float(0.99999999999999709182);

let p =
  [|
    57.156235665862923517,
    (-59.597960355475491248),
    14.136097974741747174,
    (-0.49191381609762019978),
    0.33994649984811888699e-4,
    0.46523628927048575665e-4,
    (-0.98374475304879564677e-4),
    0.15808870322491248884e-3,
    (-0.21026444172410488319e-3),
    0.21743961811521264320e-3,
    (-0.16431810653676389022e-3),
    0.84418223983852743293e-4,
    (-0.26190838401581408670e-4),
    0.36899182659531622704e-5,
  |]
  ->Belt.Array.map(Q.of_float);

let qHalf = Q.of_ints(1, 2);
let q2 = Q.of_int(2);

let sqrt2Pi = Pow.sqrt(`Real((q2, Pi)));

let gamma = (x: value): value =>
  switch (x) {
  | `Zero => `NaN
  | `Real(isInt, Unit) when QUtil.isInt(isInt) =>
    let fact = ref(Z.one);
    for (mul in 2 to Z.to_int(isInt->Q.num) - 1) {
      fact := Z.mul(fact^, Z.of_int(mul));
    };
    `Real((Q.of_bigint(fact^), Unit));
  | `Real(gtZero, reC) when Q.(gtZero > zero) =>
    /* See https://github.com/josdejong/mathjs/blob/c5971b371a5610caf37de0d6507a1c7150280f09/src/function/probability/gamma.js */
    let n = Q.(QCUtil.toQ(gtZero, reC) - one);
    let x =
      p
      ->Belt.Array.reduceWithIndex(p0, (accum, pi, i) =>
          Q.(accum + pi / (n + of_int(i)))
        );
    let t = `Real((Q.(n + g + qHalf), Constant.Unit));
    let n = `Real((Q.(n + qHalf), Constant.Unit));
    let x = `Real((x, Constant.Unit));
    BasicMath.(sqrt2Pi * Pow.pow(t, n) * Exp.exp(- t) * x);
  | (`Imag(_) | `Complex(_)) as xV =>
    let (nRe, nIm) =
      switch (xV) {
      | `Imag(imQ, imC) => (Q.zero, QCUtil.toQ(imQ, imC))
      | `Complex(reQ, reC, imQ, imC) => (
          Q.(QCUtil.toQ(reQ, reC) - one),
          QCUtil.toQ(imQ, imC),
        )
      };
    let (xRe, xIm) =
      p
      ->Belt.Array.reduceWithIndex(
          (p0, Q.zero),
          ((re, im), pi, i) => {
            let real = Q.(nRe + of_int(i));
            let deno = Q.(real * real + nIm * nIm);
            if (Q.(deno != zero)) {
              (Q.(re + pi * real / deno), Q.(im + - (pi * nIm) / deno));
            } else if (Q.(pi < zero)) {
              (Q.inf, im);
            } else {
              (Q.minus_inf, im);
            };
          },
        );

    let t =
      `Complex((Q.(nRe + g + qHalf), Constant.Unit, nIm, Constant.Unit));
    let nRe = Q.(nRe + qHalf);

    let result =
      switch (nRe->Q.classify) {
      | INF
      | MINF => `Zero
      | _ =>
        let n = `Complex((nRe, Constant.Unit, nIm, Constant.Unit));
        BasicMath.(Pow.pow(t, n) * sqrt2Pi);
      };

    let t = Exp.exp(BasicMath.(- t));
    let x = `Complex((xRe, Constant.Unit, xIm, Constant.Unit));

    BasicMath.(result * t * x);
  | _ => `NaN
  };

let factorial = x => BasicMath.(x + one)->gamma;
