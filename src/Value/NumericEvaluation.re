open Types;

let reduceRange = (a, b, initialValue, f, iteratee) =>
  switch (toInt(a), toInt(b)) {
  | (Some(a), Some(b)) when b >= a =>
    let current = ref(initialValue);
    for (i in a to b) {
      current := iteratee(current^, f(ofInt(i)));
    };
    current^;
  | _ => nan
  };

let sum = (f, a, b) => reduceRange(a, b, zero, f, Value.add);
let product = (f, a, b) => reduceRange(a, b, one, f, Value.mul);

let derivative = (f, x) => {
  open BasicMath;

  let h = real(Q.of_int(1_000)->Q.inv);
  let _2h = h * ofInt(2);
  let _8 = ofInt(8);

  let (pL1, pL2) = (f(x - h), f(x - _2h));
  let (pR1, pR2) = (f(x + h), f(x + _2h));

  let factors = pL2 - _8 * pL1 + _8 * pR1 - pR2;
  factors / (ofInt(12) * h);
};

let integrate = (f: value => value, a, b) => {
  let (a, b) = (Types.toQ(a), Types.toQ(b));

  if (Q.(b > a)) {
    let n = 100;
    let n2 = n * 2;
    let _2 = Q.of_int(2);
    let _4 = Q.of_int(4);
    let h = Q.((b - a) / (of_int(n) * _2));

    let sum = Q.(real(a)->f->toQ + real(b)->f->toQ)->ref;

    let i = ref(1);
    while (i^ < n2 && Q.(sum^ != undef)) {
      let v = Q.(a + of_int(i^) * h)->real;
      sum := Q.(sum^ + _4 * f(v)->toQ);
      i := i^ + 2;
    };

    i := 2;
    while (i^ < n2 - 1 && Q.(sum^ != undef)) {
      let v = Q.(a + of_int(i^) * h)->real;
      sum := Q.(sum^ + _2 * f(v)->toQ);
      i := i^ + 2;
    };

    if (Q.(sum^ != undef)) {
      Q.(sum^ * h / of_int(3))->real;
    } else {
      nan;
    };
  } else {
    nan;
  };
};
