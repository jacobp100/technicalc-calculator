open Types;

let derivative = (f, x) => {
  open Base;

  let h = real(Rational(1, 1000, Unit));
  let _2h = h * ofInt(2);
  let _8 = ofInt(8);

  let (pL1, pL2) = (f(x - h), f(x - _2h));
  let (pR1, pR2) = (f(x + h), f(x + _2h));

  let factors = pL2 - _8 * pL1 + _8 * pR1 - pR2;
  factors / (ofInt(12) * h);
};

let integrate = (f: value => value, a, b) => {
  let (a, b) = (Types.toFloat(a), Types.toFloat(b));

  if (b > a) {
    let n = 100.;
    let n2 = n *. 2.;
    let h = (b -. a) /. (n *. 2.);

    let sum = Types.(ofFloat(a)->f->toFloat +. ofFloat(b)->f->toFloat)->ref;

    let i = ref(1.);
    while (i^ < n2 && FloatUtil.isFinite(sum^)) {
      let v = ofFloat(a +. i^ *. h);
      sum := sum^ +. 4. *. f(v)->toFloat;
      i := i^ +. 2.;
    };

    i := 2.;
    while (i^ < n2 -. 1. && FloatUtil.isFinite(sum^)) {
      let v = ofFloat(a +. i^ *. h);
      sum := sum^ +. 2. *. f(v)->toFloat;
      i := i^ +. 2.;
    };

    if (FloatUtil.isFinite(sum^)) {
      ofFloat(sum^ *. h /. 3.);
    } else {
      nan;
    };
  } else {
    nan;
  };
};