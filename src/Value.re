module Make = (Number: Types.Scalar) : Types.Scalar => {
  module ScalarMatrix = Matrix.Make(Number);

  type t = [ | `Scalar(Number.t) | `Matrix(ScalarMatrix.t) | `NaN];

  let nan = `NaN;
  let is_nan = a => a == `NaN;

  let zero = `Scalar(Number.zero);
  let is_zero = (==)(zero);

  let normalize = a =>
    switch (a) {
    | `Scalar(aS) => Number.is_nan(aS) ? `NaN : a
    | `Matrix(aM) => ScalarMatrix.is_nan(aM) ? `NaN : a
    | `NaN => nan
    };

  let of_scalar = a => normalize(`Scalar(a));
  let of_matrix = a => normalize(`Matrix(a));

  let add = (a, b) =>
    switch (a, b) {
    | (`Scalar(aS), `Scalar(bS)) => of_scalar(Number.add(aS, bS))
    | (`Matrix(aM), `Matrix(bM)) => of_matrix(ScalarMatrix.add(aM, bM))
    | _ => nan
    };

  let sub = (a, b) =>
    switch (a, b) {
    | (`Scalar(aS), `Scalar(bS)) => of_scalar(Number.sub(aS, bS))
    | (`Matrix(aM), `Matrix(bM)) => of_matrix(ScalarMatrix.sub(aM, bM))
    | _ => nan
    };

  let mul = (a, b) =>
    switch (a, b) {
    | (`Scalar(aS), `Scalar(bS)) => of_scalar(Number.mul(aS, bS))
    | (`Matrix(aM), `Matrix(bM)) => of_matrix(ScalarMatrix.mul(aM, bM))
    | (`Scalar(aS), `Matrix(aM))
    | (`Matrix(aM), `Scalar(aS)) =>
      of_matrix(ScalarMatrix.mul_const(aS, aM))
    | _ => nan
    };

  let div = (a, b) =>
    switch (a, b) {
    | (`Scalar(aS), `Scalar(bS)) => of_scalar(Number.div(aS, bS))
    | (`Matrix(aM), `Matrix(bM)) => of_matrix(ScalarMatrix.div(aM, bM))
    | (`Matrix(aM), `Scalar(aS)) =>
      of_matrix(ScalarMatrix.div_const(aS, aM))
    | _ => nan
    };

  let pow = (a, b) =>
    switch (a, b) {
    | (`Scalar(aS), `Scalar(bS)) => of_scalar(Number.pow(aS, bS))
    | _ => nan
    };

  let neg = a =>
    switch (a) {
    | `Scalar(aS) => `Scalar(Number.neg(aS))
    | `Matrix(aM) => `Matrix(ScalarMatrix.neg(aM))
    | _ => nan
    };

  let _map_scalar = (iteratee, a) =>
    switch (a) {
    | `Scalar(aS) => `Scalar(iteratee(aS))
    | _ => nan
    };

  let exp = _map_scalar(Number.exp);
  let sin = _map_scalar(Number.sin);
  let cos = _map_scalar(Number.cos);
  let tan = _map_scalar(Number.tan);
  let log = _map_scalar(Number.log);
  let sqrt = _map_scalar(Number.sqrt);
};
