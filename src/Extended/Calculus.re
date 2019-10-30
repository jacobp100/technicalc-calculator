open Types;

let derivative = (f, x) => {
  open Base;

  let h = ofFloat(1e-4);
  let _2h = h * ofInt(2);
  let _8 = ofInt(8);

  let (pL1, pL2) = (f(x - h), f(x - _2h));
  let (pR1, pR2) = (f(x + h), f(x + _2h));

  let factors = pL2 - _8 * pL1 + _8 * pR1 - pR2;
  factors / (ofInt(12) * h);
};

let integrate = (f: value => value, a, b) => {
  open Decimal;
  let (a, b) = (Types.toDecimal(a), Types.toDecimal(b));

  if (b > a) {
    let _2 = ofInt(2);
    let _4 = ofInt(4);
    let n = ofInt(100);
    let n2 = n * _2;
    let h = (b - a) / (n * _2);

    let sum =
      Types.(ofDecimal(a)->f->toDecimal + ofDecimal(b)->f->toDecimal)->ref;

    let i = ref(one);
    while (i^ < n2 && isFinite(sum^)) {
      let v = ofDecimal(a + i^ * h);
      sum := sum^ + _4 * f(v)->toDecimal;
      i := i^ + _2;
    };

    i := _2;
    while (i^ < n2 - one && isFinite(sum^)) {
      let v = ofDecimal(a + i^ * h);
      sum := sum^ + _2 * f(v)->toDecimal;
      i := i^ + _2;
    };

    if (isFinite(sum^)) {
      ofDecimal(sum^ * h / ofInt(3));
    } else {
      Types.nan;
    };
  } else {
    Types.nan;
  };
};