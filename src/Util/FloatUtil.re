// Checks int boundaries too
let isInt = f => f->int_of_float->float_of_int == f;

let toInt = f =>
  f->int_of_float->float_of_int == f ? Some(f->int_of_float) : None;

let isFinite = f =>
  switch (classify_float(f)) {
  | FP_zero
  | FP_normal
  | FP_subnormal => true
  | FP_infinite
  | FP_nan => false
  };