module SciLineValue = Value.Make(Complex);

include SciLineValue;

let of_real = x => {
  let complex = x |> Int64.of_int |> Real.of_int64 |> Complex.of_real;
  `Scalar(complex);
};

let float_of_value = x =>
  switch (x) {
  | `Scalar(xS) => Complex.float_of_complex(xS)
  | _ => Pervasives.nan
  };
