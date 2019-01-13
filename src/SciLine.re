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

let resolve = a => Resolved(SciLineAst.eval(a));

/* FIXME */
let of_float = value_of_float;

let _number_of = a =>
  switch (a) {
  | Resolved(v) =>
    switch (SciLineValue.to_number(v)) {
    | Some(comp) => Some(comp)
    | None => None
    }
  | _ => None
  };

let to_float = a =>
  switch (_number_of(a)) {
  | Some(comp) => Complex.to_float(comp)
  | None => nan
  };

let to_floats = a =>
  switch (_number_of(a)) {
  | Some(comp) => Complex.to_floats(comp)
  | None => (nan, nan)
  };

let is_nan = a =>
  switch (_number_of(a)) {
  | Some(comp) => Complex.is_nan(comp)
  | None => true
  };

let format_resolved = (format, a) =>
  switch (a) {
  | Resolved(a) => format(a)
  | _ => "Unresolved type"
  };

let to_string = format_resolved(SciLineValue.to_string);

let to_latex = format_resolved(SciLineValue.to_latex);
