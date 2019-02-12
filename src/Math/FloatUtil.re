let pi = 4.0 *. atan(1.0);
let asinh = f => log(f +. sqrt(f *. f +. 1.0));
let acosh = f => log(f +. sqrt(f *. f -. 1.0));
let atanh = f => log((1.0 +. f) /. (1.0 -. f)) /. 2.0;

let is_int = f => floor(f) == f;

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
  | (0, 0) => `BothBound
  | (0, _) => `LowerBound
  | (_, 0) => `UpperBound
  | ((-1), 1) => `Inside(f)
  | _ => `Outside
  };
};
