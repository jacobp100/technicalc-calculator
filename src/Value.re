module Make = (Number: Types.Scalar) => {
  module NumberMatrix = Matrix.Make(Number);

  type t =
    | Scalar(Number.t)
    | Matrix(NumberMatrix.t)
    | NaN;

  let nan = NaN;
  let zero = Scalar(Number.zero);
  let one = Scalar(Number.one);
  let minus_one = Scalar(Number.minus_one);
  let e = Scalar(Number.e);
  let pi = Scalar(Number.pi);

  let is_nan = a => Pervasives.(==)(a, NaN);

  let to_int = a =>
    switch (a) {
    | Scalar(aS) => Number.to_int(aS)
    | _ => None
    };

  let to_number = a =>
    switch (a) {
    | Scalar(aS) => Some(aS)
    | _ => None
    };

  let to_matrix = a =>
    switch (a) {
    | Matrix(aM) => Some(aM)
    | _ => None
    };

  let normalize = a =>
    switch (a) {
    | Scalar(aS) => Number.is_nan(aS) ? NaN : a
    | Matrix(aM) => NumberMatrix.is_nan(aM) ? NaN : a
    | NaN => nan
    };

  let of_scalar = a => normalize(Scalar(a));
  let of_matrix = a => normalize(Matrix(a));

  let of_number = a => of_scalar(a);
  let of_int = a => of_scalar(Number.of_int(a));
  let of_float = a => of_scalar(Number.of_float(a));
  let of_string = a => of_scalar(Number.of_string(a));
  let of_matrix_elements = (~rows, ~columns, elements) => {
    let to_scalar = element =>
      switch (element) {
      | Scalar(aS) => aS
      | _ => Number.nan
      };
    let elements = Array.map(to_scalar, elements);
    /* Normalized to NaN if non-scalar values exist */
    of_matrix(NumberMatrix.from_elements(~rows, ~columns, elements));
  };

  let equal = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => Number.equal(aS, bS)
    | (Matrix(aM), Matrix(bM)) => NumberMatrix.equal(aM, bM)
    | _ => false
    };

  let add = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => of_scalar(Number.add(aS, bS))
    | (Matrix(aM), Matrix(bM)) => of_matrix(NumberMatrix.add(aM, bM))
    | _ => nan
    };

  let sub = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => of_scalar(Number.sub(aS, bS))
    | (Matrix(aM), Matrix(bM)) => of_matrix(NumberMatrix.sub(aM, bM))
    | _ => nan
    };

  let mul = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => of_scalar(Number.mul(aS, bS))
    | (Matrix(aM), Matrix(bM)) => of_matrix(NumberMatrix.mul(aM, bM))
    | (Scalar(aS), Matrix(aM))
    | (Matrix(aM), Scalar(aS)) => of_matrix(NumberMatrix.mul_const(aM, aS))
    | _ => nan
    };

  let div = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => of_scalar(Number.div(aS, bS))
    | (Matrix(aM), Scalar(aS)) => of_matrix(NumberMatrix.div_const(aM, aS))
    | _ => nan
    };

  let pow = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => of_scalar(Number.pow(aS, bS))
    | (Matrix(aM), Scalar(aS)) => of_matrix(NumberMatrix.pow_const(aM, aS))
    | _ => nan
    };

  let neg = a =>
    switch (a) {
    | Scalar(aS) => of_scalar(Number.neg(aS))
    | Matrix(aM) => of_matrix(NumberMatrix.neg(aM))
    | _ => nan
    };

  let dot = (a, b) =>
    switch (a, b) {
    | (Scalar(aS), Scalar(bS)) => of_scalar(Number.mul(aS, bS))
    | (Matrix(aM), Matrix(bM)) => of_scalar(NumberMatrix.dot(aM, bM))
    | _ => nan
    };

  let abs = a =>
    switch (a) {
    | Scalar(aS) => of_scalar(Number.abs(aS))
    | Matrix(aM) => of_scalar(NumberMatrix.det(aM))
    | _ => nan
    };

  let _map_scalar = (iteratee, a) =>
    switch (a) {
    | Scalar(aS) => of_scalar(iteratee(aS))
    | _ => nan
    };

  let sqrt = _map_scalar(Number.sqrt);
  let exp = _map_scalar(Number.exp);
  let log = _map_scalar(Number.log);
  let factorial = _map_scalar(Number.factorial);
  let sin = _map_scalar(Number.sin);
  let arcsin = _map_scalar(Number.arcsin);
  let sinh = _map_scalar(Number.sinh);
  let arcsinh = _map_scalar(Number.arcsinh);
  let cos = _map_scalar(Number.cos);
  let arccos = _map_scalar(Number.arccos);
  let cosh = _map_scalar(Number.cosh);
  let arccosh = _map_scalar(Number.arccosh);
  let tan = _map_scalar(Number.tan);
  let arctan = _map_scalar(Number.arctan);
  let tanh = _map_scalar(Number.tanh);
  let arctanh = _map_scalar(Number.arctanh);

  let to_string = (~format=OutputFormat.default, x) =>
    switch (x) {
    | Scalar(xS) => Number.to_string(~format, xS)
    | Matrix(xM) => NumberMatrix.to_string(~format, xM)
    | NaN => "NaN"
    };
};
