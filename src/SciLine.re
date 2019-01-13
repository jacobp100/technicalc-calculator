module SciLineValue = Value.Make(Complex);
module SciLineAst = AST.Make(SciLineValue);

include SciLineAst;

let zero = value_of_t(SciLineValue.zero);
let one = value_of_t(SciLineValue.one);
let minus_one = value_of_t(SciLineValue.minus_one);
let pi = value_of_t(SciLineValue.pi);
let e = value_of_t(SciLineValue.e);
let i = value_of_t(SciLineValue.of_number(Complex.i));
let minus_i = value_of_t(SciLineValue.of_number(Complex.minus_i));

[@bs.deriving abstract]
type wrappedValue = {
  _typeof: string,
  value: SciLineValue.t,
};

exception Not_Resolved;

let _unwrap_resolved_value = wrapper => {
  switch (_typeofGet(wrapper)) {
  | "resolved" => valueGet(wrapper)
  | _ => raise(Not_Resolved)
  };
};

let _wrap_resolved_value = value => wrappedValue(~_typeof="resolved", ~value);

let resolve = a => _wrap_resolved_value(eval(a));

let resolveWithContext = (jsContext, a) => {
  let context =
    Array.fold_left(
      (accum, (key, value)) =>
        Context.add(key, _unwrap_resolved_value(value), accum),
      Context.empty,
      Js.Dict.entries(jsContext),
    );
  _wrap_resolved_value(eval(~context, a));
};

/* FIXME */
let of_float = value_of_float;

let to_float = a =>
  switch (SciLineValue.to_number(_unwrap_resolved_value(a))) {
  | Some(comp) => Complex.to_float(comp)
  | None => nan
  };

let to_floats = a =>
  switch (SciLineValue.to_number(_unwrap_resolved_value(a))) {
  | Some(comp) => Complex.to_floats(comp)
  | None => (nan, nan)
  };

let to_matrix = a =>
  switch (SciLineValue.to_matrix(a)) {
  | Some(mat) => Matrix.to_array_matrix(mat)
  | None => 0
  };

let is_nan = a => SciLineValue.is_nan(_unwrap_resolved_value(a));

let to_string = a => SciLineValue.to_string(_unwrap_resolved_value(a));

let to_latex = a => SciLineValue.to_latex(_unwrap_resolved_value(a));
