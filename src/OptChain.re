let (|?) = (x, f) =>
  switch (x) {
  | Some(a) => Some(f(a))
  | None => None
  };
