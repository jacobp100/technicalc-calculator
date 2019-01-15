module SciLineValue = Value.Make(Complex);
module NumberMatrix = Matrix.Make(Complex);
module SciLineAst = AST.Make(SciLineValue);

module Result =
  FFITypeCheck.Make({
    let key = "resolved";
    type t = SciLineValue.t;
  });

include SciLineAst;

let zero = value_of_t(SciLineValue.zero);
let one = value_of_t(SciLineValue.one);
let minus_one = value_of_t(SciLineValue.minus_one);
let pi = value_of_t(SciLineValue.pi);
let e = value_of_t(SciLineValue.e);
let i = value_of_t(SciLineValue.of_number(Complex.i));
let minus_i = value_of_t(SciLineValue.of_number(Complex.minus_i));

let _convert_context = jsContext =>
  Array.fold_left(
    (accum, (key, value)) => Context.add(key, Result.unwrap(value), accum),
    Context.empty,
    Js.Dict.entries(jsContext),
  );

let resolve = a => Result.wrap(eval(a));
let resolveWithContext = (jsContext, a) =>
  Result.wrap(eval(~context=_convert_context(jsContext), a));

let of_float = value_of_float;
let of_complex_floats = (re, im) =>
  value_of_t(SciLineValue.of_number(Complex.of_floats(re, im)));

let to_float = a =>
  switch (SciLineValue.to_number(Result.unwrap(a))) {
  | Some(comp) => Complex.to_float(comp)
  | None => nan
  };

let to_complex_floats = a =>
  switch (SciLineValue.to_number(Result.unwrap(a))) {
  | Some(comp) => Complex.to_floats(comp)
  | None => (nan, nan)
  };

let _map_matrix = (fn, a) =>
  switch (SciLineValue.to_matrix(Result.unwrap(a))) {
  | Some(mat) => Array.map(Array.map(fn), NumberMatrix.to_matrix(mat))
  | None => [||]
  };

let to_floats_matrix = _map_matrix(Complex.to_float);
let to_complex_floats_matrix = _map_matrix(Complex.to_floats);

let is_nan = a => SciLineValue.is_nan(Result.unwrap(a));
let to_string = a => SciLineValue.to_string(Result.unwrap(a));
let to_latex = a => SciLineValue.to_latex(Result.unwrap(a));
