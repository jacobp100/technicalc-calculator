module Make = (Number: Types.Scalar) => {
  module NumberMatrix = Matrix.Make(Number);

  type t =
    | Scalar(Number.t)
    | Matrix(NumberMatrix.t)
    | NaN;

  type encoding = [
    | `Scalar(Number.encoding)
    | `Matrix(NumberMatrix.encoding)
    | `NaN
    | `UnknownValue
  ];

  let nan = NaN;
  let zero = Scalar(Number.zero);
  let one = Scalar(Number.one);
  let minusOne = Scalar(Number.minusOne);
  let e = Scalar(Number.e);
  let pi = Scalar(Number.pi);

  let isNan = a => Pervasives.(==)(a, NaN);

  let toInt = a =>
    switch (a) {
    | Scalar(aS) => Number.toInt(aS)
    | _ => None
    };

  let toNumber = a =>
    switch (a) {
    | Scalar(aS) => Some(aS)
    | _ => None
    };

  let toMatrix = a =>
    switch (a) {
    | Matrix(aM) => Some(aM)
    | _ => None
    };

  let normalize = a =>
    switch (a) {
    | Scalar(aS) => Number.isNan(aS) ? NaN : a
    | Matrix(aM) => NumberMatrix.isNan(aM) ? NaN : a
    | NaN => nan
    };

  let ofScalar = a => normalize(Scalar(a));
  let ofMatrix = a => normalize(Matrix(a));

  let ofNumber = a => ofScalar(a);
  let ofInt = a => ofScalar(Number.ofInt(a));
  let ofFloat = a => ofScalar(Number.ofFloat(a));
  let ofString = a => ofScalar(Number.ofString(a));
  let ofStringBase = (base, a) => ofScalar(Number.ofStringBase(base, a));
  let ofMatrixElements = (~rows, ~columns, elements) => {
    let toScalar = element =>
      switch (element) {
      | Scalar(aS) => aS
      | _ => Number.nan
      };
    let elements = Belt.Array.map(elements, toScalar);
    /* Normalized to NaN if non-scalar values exist */
    ofMatrix(NumberMatrix.fromElements(~rows, ~columns, elements));
  };

  let equal = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => Number.equal(aS, bS)
    | (Matrix(aM), Matrix(bM)) => NumberMatrix.equal(aM, bM)
    | _ => false
    };

  let add = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => ofScalar(Number.add(aS, bS))
    | (Matrix(aM), Matrix(bM)) => ofMatrix(NumberMatrix.add(aM, bM))
    | _ => nan
    };

  let sub = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => ofScalar(Number.sub(aS, bS))
    | (Matrix(aM), Matrix(bM)) => ofMatrix(NumberMatrix.sub(aM, bM))
    | _ => nan
    };

  let mul = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => ofScalar(Number.mul(aS, bS))
    | (Matrix(aM), Matrix(bM)) => ofMatrix(NumberMatrix.mul(aM, bM))
    | (Scalar(aS), Matrix(aM))
    | (Matrix(aM), Scalar(aS)) => ofMatrix(NumberMatrix.mulConst(aM, aS))
    | _ => nan
    };

  let div = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => ofScalar(Number.div(aS, bS))
    | (Matrix(aM), Scalar(aS)) => ofMatrix(NumberMatrix.divConst(aM, aS))
    | _ => nan
    };

  let pow = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => ofScalar(Number.pow(aS, bS))
    | (Matrix(aM), Scalar(aS)) => ofMatrix(NumberMatrix.powConst(aM, aS))
    | _ => nan
    };

  let neg = a =>
    switch (a) {
    | Scalar(aS) => ofScalar(Number.neg(aS))
    | Matrix(aM) => ofMatrix(NumberMatrix.neg(aM))
    | _ => nan
    };

  let dot = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => ofScalar(Number.mul(aS, bS))
    | (Matrix(aM), Matrix(bM)) => ofScalar(NumberMatrix.dot(aM, bM))
    | _ => nan
    };

  let abs = a =>
    switch (a) {
    | Scalar(aS) => ofScalar(Number.abs(aS))
    | Matrix(aM) => ofScalar(NumberMatrix.det(aM))
    | _ => nan
    };

  let _mapScalar = (iteratee, a) =>
    switch (a) {
    | Scalar(aS) => ofScalar(iteratee(aS))
    | _ => nan
    };

  let sqrt = _mapScalar(Number.sqrt);
  let exp = _mapScalar(Number.exp);
  let log = _mapScalar(Number.log);
  let factorial = _mapScalar(Number.factorial);
  let sin = _mapScalar(Number.sin);
  let asin = _mapScalar(Number.asin);
  let sinh = _mapScalar(Number.sinh);
  let asinh = _mapScalar(Number.asinh);
  let cos = _mapScalar(Number.cos);
  let acos = _mapScalar(Number.acos);
  let cosh = _mapScalar(Number.cosh);
  let acosh = _mapScalar(Number.acosh);
  let tan = _mapScalar(Number.tan);
  let atan = _mapScalar(Number.atan);
  let tanh = _mapScalar(Number.tanh);
  let atanh = _mapScalar(Number.atanh);

  let toString = (~format=OutputFormat.default, x) =>
    switch (format.mode, x) {
    | (_, Scalar(xS)) => Number.toString(~format, xS)
    | (_, Matrix(xM)) => NumberMatrix.toString(~format, xM)
    | (String | Tex, NaN) => "NaN"
    | (MathML, NaN) => "<mi>NaN</mi>"
    };

  let encode = x =>
    switch (x) {
    | Scalar(s) => `Scalar(Number.encode(s))
    | Matrix(m) => `Matrix(NumberMatrix.encode(m))
    | NaN => `NaN
    };
  let decode = x =>
    switch (x) {
    | `Scalar(s) => Scalar(Number.decode(s))
    | `Matrix(m) => Matrix(NumberMatrix.decode(m))
    | _ => NaN
    };
};
