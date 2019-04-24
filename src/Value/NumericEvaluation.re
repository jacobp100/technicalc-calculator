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

  let h = real(Q.of_int(1_000_000)->Q.inv);
  let h2 = h * ofInt(2);
  let _8 = ofInt(8);

  let (pL2, pL1) = (f(x - h2), f(x - h));
  let (pR2, pR1) = (f(x + h2), f(x + h));

  let factors = pL2 - _8 * pL1 + _8 * pR2 - pR1;
  factors / (ofInt(12) * h);
};

let integrate = (f: value => value, a, b) =>
  switch (Types.toQ(a), Types.toQ(b)) {
  | (Some(a), Some(b)) when Q.(b > a) =>
    let range = Q.(b - a);
    let maxN = 87;
    let n = Q.(range > Q.of_int(maxN)) ? maxN : QUtil.floor(range)->Z.to_int;
    let n2 = n * 2;
    let _2 = Q.of_int(2);
    let _4 = Q.of_int(4);
    let h = Q.(range / (of_int(n) * _2));

    let sum =
      switch (real(a)->f->toQ, real(b)->f->toQ) {
      | (Some(a), Some(b)) => Q.(a + b)
      | _ => Q.undef
      };
    let sum = ref(sum);

    let i = ref(1);
    while (i^ < n2 && Q.(sum^ != undef)) {
      let v = Q.(a + of_int(i^) * h)->real;

      switch (f(v)->toQ) {
      | Some(q) =>
        sum := Q.(sum^ + _4 * q);
        i := i^ + 2;
      | _ => sum := Q.undef
      };
    };

    let i = ref(2);
    while (i^ < n2 - 1 && Q.(sum^ != undef)) {
      let v = Q.(a + of_int(i^) * h)->real;

      switch (f(v)->toQ) {
      | Some(q) =>
        sum := Q.(sum^ + _2 * q);
        i := i^ + 2;
      | _ => sum := Q.undef
      };
    };

    if (Q.(sum^ != undef)) {
      Q.(sum^ * h / of_int(3))->real;
    } else {
      nan;
    };
  | (Some(a), Some(b)) when Q.(b == a) => zero
  | _ => nan
  };
