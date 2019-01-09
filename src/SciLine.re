module SciLineValue = Value.Make(Complex);

include SciLineValue;

let of_float = x => {
  let complex = x |> Real.of_float |> Complex.of_real;
  `Scalar(complex);
};

let pi = `Scalar(Complex.of_real(Real.pi));

let i = `Scalar(Complex.of_imaginary(Real.of_int64(1L)));

let float_of_value = x =>
  switch (x) {
  | `Scalar(xS) => Complex.float_of_complex(xS)
  | _ => Pervasives.nan
  };

let complex_tuple = x =>
  switch (x) {
  | `Scalar(xS) => Complex.tuple_of_complex(xS)
  | _ => (Pervasives.nan, Pervasives.nan)
  };

let string_of_value = x =>
  switch (x) {
  | `Scalar(xS) => Complex.to_string(xS)
  | _ => "other"
  };
