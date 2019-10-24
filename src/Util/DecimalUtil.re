let magnitude = f =>
  if (f != 0.) {
    abs_float(f)->log10->floor->int_of_float;
  } else {
    0;
  };