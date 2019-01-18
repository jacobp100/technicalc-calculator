let default = (defaultValue, arg) =>
  switch (arg) {
  | Some(v) => v
  | None => defaultValue
  };

type boundary =
  | BothBound
  | UpperBound
  | LowerBound
  | Inside(float)
  | Outside;

let bounds = (~lower=?, ~upper=?, f) => {
  let lowerCompare =
    switch (lower) {
    | Some(l) => compare(l, f)
    | None => (-1)
    };
  let upperCompare =
    switch (upper) {
    | Some(u) => compare(u, f)
    | None => 1
    };
  switch (lowerCompare, upperCompare) {
  | (0, 0) => BothBound
  | (0, _) => LowerBound
  | (_, 0) => UpperBound
  | ((-1), 1) => Inside(f)
  | _ => Outside
  };
};
