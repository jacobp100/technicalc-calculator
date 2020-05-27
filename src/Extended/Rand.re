open Types;

let rand = () => Random.float(1.)->ofFloat;

let randInt = (a, b) =>
  switch (a->toInt, b->toInt) {
  | (Some(a), Some(b)) => (a + Random.int(abs(b - a)))->ofInt
  | _ => nan
  };