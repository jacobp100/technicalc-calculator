module SciLineValue = Value.Make(Complex);

include SciLineValue;

let of_float = re => `Scalar(Complex.of_float(re));
let of_floats = (re, im) => `Scalar(Complex.of_floats(re, im));

let zero = `Scalar(Complex.zero);
let one = `Scalar(Complex.one);
let minus_one = `Scalar(Complex.minus_one);
let e = `Scalar(Complex.e);
let i = `Scalar(Complex.i);
let minus_i = `Scalar(Complex.minus_i);

let to_float = x =>
  switch (x) {
  | `Scalar(xS) => Complex.to_float(xS)
  | _ => Pervasives.nan
  };

let to_floats = x =>
  switch (x) {
  | `Scalar(xS) => Complex.to_floats(xS)
  | _ => (Pervasives.nan, Pervasives.nan)
  };

let to_string = x =>
  switch (x) {
  | `Scalar(xS) => Complex.to_string(xS)
  | `Matrix(_) => "matrix"
  | `NaN => "NaN"
  };
