include SciLine;

let ofComplexFloats = (re, im) =>
  Types.complex(Q.of_float(re), Q.of_float(im))->Value.encode->ofEncoded;

let toComplexFloats = a: (float, float) =>
  switch (a) {
  | `Zero => (0., 0.)
  | `Real(q, c) => (QCUtil.toFloat(q, c), 0.)
  | `Imag(q, c) => (0., QCUtil.toFloat(q, c))
  | `Complex(reQ, reC, imQ, imC) => (
      QCUtil.toFloat(reQ, reC),
      QCUtil.toFloat(imQ, imC),
    )
  | _ => (nan, nan)
  };

let _mapMatrix = (fn: Types.value => 'a, a: Types.value): array(array('a)) => {
  let fn = x => Types.valueOfScalar(x)->fn;

  switch (a) {
  | `Vector2(a, b) => [|[|fn(a)|], [|fn(b)|]|]
  | `Vector3(a, b, c) => [|[|fn(a)|], [|fn(b)|], [|fn(c)|]|]
  | `Matrix2(a, b, c, d) => [|[|fn(a), fn(b)|], [|fn(c), fn(d)|]|]
  | `Matrix3(a, b, c, d, e, f, g, h, i) => [|
      [|fn(a), fn(b), fn(c)|],
      [|fn(d), fn(e), fn(f)|],
      [|fn(g), fn(h), fn(i)|],
    |]
  | _ => invalid_arg("Not a matrix")
  };
};

let toFloatsMatrix = _mapMatrix(Types.toFloat);
let toComplexFloatsMatrix = _mapMatrix(toComplexFloats);
