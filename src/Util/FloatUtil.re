let isInt = f => floor(f) == f;

let isFinite = f =>
  switch (classify_float(f)) {
  | FP_zero
  | FP_normal
  | FP_subnormal => true
  | FP_infinite
  | FP_nan => false
  };