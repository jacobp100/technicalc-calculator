/* Disable ops that can overflow */
let (+) = None;
let (-) = None;
let ( * ) = None;
let (~-) = None;

module I64Ops = {
  include Int64;
  let (+) = add;
  let (-) = sub;
  let ( * ) = mul;
  let (/) = div;
  let (~-) = neg;
};

module SafeOps = {
  let int64FitsInt = n => I64Ops.(compare(of_int(to_int(n)), n) === 0);

  let toInt = n => int64FitsInt(n) ? Some(I64Ops.to_int(n)) : None;

  let neg = a => I64Ops.(- of_int(a))->toInt;
  let abs = a => I64Ops.(of_int(a)->abs)->toInt;
  let add = (a, b) => I64Ops.(of_int(a) + of_int(b))->toInt;
  let sub = (a, b) => I64Ops.(of_int(a) - of_int(b))->toInt;
  let mul = (a, b) => I64Ops.(of_int(a) * of_int(b))->toInt;
};

type t =
  | Rational(int, int, Real_Constant.t)
  | Float(float);

let ratFloat = (n, d, c) =>
  float_of_int(n) /. float_of_int(d) *. Real_Constant.toFloat(c);

let one = Rational(1, 1, Unit);
let minusOne = Rational(-1, 1, Unit);
let zero = Rational(0, 1, Unit);
let nan = Rational(1, 0, Unit);
let equal = (a, b) => a == b;
let isNaN = _ => false;

let toFloat = a =>
  switch (a) {
  | Rational(n, d, c) => ratFloat(n, d, c)
  | Float(f) => f
  };

let rec gcd64 = (a, b) => b == 0L ? a : gcd64(b, Int64.rem(a, b));

let rat64 = (n, d, c) => {
  open I64Ops;
  let gcd = gcd64(n, d);
  let n = n / gcd;
  let d = d / gcd;
  if (SafeOps.int64FitsInt(n) && SafeOps.int64FitsInt(d)) {
    Rational(to_int(n), to_int(d), c);
  } else {
    Float(to_float(n) /. to_float(d));
  };
};

let ratConstExp = (n, d, c) =>
  switch (Real_Constant.simplifyExp(c)) {
  | `None => Rational(n, d, Exp(c))
  | `Factor(n', c) => I64Ops.(rat64(of_int(n) * of_int(n'), of_int(d), c))
  };

let ratConstSqrt = (n, d, c) =>
  switch (Real_Constant.simplifySqrt(c)) {
  | `None => Rational(n, d, Sqrt(c))
  | `Zero => Rational(0, 1, Unit)
  | `Factor(n', c) => I64Ops.(rat64(of_int(n) * of_int(n'), of_int(d), c))
  };

let neg = a =>
  switch (a) {
  | Rational(n, d, c) =>
    switch (SafeOps.neg(n)) {
    | Some(n) => Rational(n, d, c)
    | _ => Float(-. ratFloat(n, d, c))
    }
  | Float(f) => Float(-. f)
  };

let abs = a =>
  switch (a) {
  | Rational(n, d, c) =>
    switch (SafeOps.abs(n)) {
    | Some(n) => Rational(n, d, c)
    | _ => Float(Pervasives.abs_float(ratFloat(n, d, c)))
    }
  | Float(f) => Float(Pervasives.abs_float(f))
  };

let add = (a, b) =>
  switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, bc)) when c == bc =>
    open I64Ops;
    let ad = of_int(ad);
    let bd = of_int(bd);
    let n = of_int(an) * bd + of_int(bn) * ad;
    let d = ad * bd;
    rat64(n, d, c);
  | (a, b) => Float(toFloat(a) +. toFloat(b))
  };

let sub = (a, b) =>
  switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, bc)) when c == bc =>
    open I64Ops;
    let ad = of_int(ad);
    let bd = of_int(bd);
    let n = of_int(an) * bd - of_int(bn) * ad;
    let d = ad * bd;
    rat64(n, d, c);
  | (a, b) => Float(toFloat(a) -. toFloat(b))
  };

let mulRat = (an, ad, bn, bd, c) => {
  open I64Ops;
  let n = of_int(an) * of_int(bn);
  let d = of_int(ad) * of_int(bd);
  rat64(n, d, c);
};

let mul = (a, b) =>
  switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, Unit))
  | (Rational(an, ad, Unit), Rational(bn, bd, c)) =>
    mulRat(an, ad, bn, bd, c)
  | (Rational(an, ad, Exp(aExp) as ac), Rational(bn, bd, Exp(bExp) as bc)) =>
    switch (mulRat(an, ad, bn, bd, Unit), SafeOps.add(aExp, bExp)) {
    | (Rational(n, d, _), Some(c)) => ratConstExp(n, d, c)
    | _ => Float(ratFloat(an, ad, ac) *. ratFloat(bn, bd, bc))
    }
  | (Rational(an, ad, Sqrt(aS) as ac), Rational(bn, bd, Sqrt(bS) as bc)) =>
    switch (mulRat(an, ad, bn, bd, Unit), SafeOps.mul(aS, bS)) {
    | (Rational(n, d, _), Some(c)) => ratConstSqrt(n, d, c)
    | _ => Float(ratFloat(an, ad, ac) *. ratFloat(bn, bd, bc))
    }
  | (a, b) => Float(toFloat(a) *. toFloat(b))
  };

let divRat = (an, ad, bn, bd, c) => {
  open I64Ops;
  let n = of_int(an) * of_int(bd);
  let d = of_int(ad) * of_int(bn);
  rat64(n, d, c);
};

let div = (a, b) =>
  switch (a, b) {
  | (_, Rational(0, _, _)) => nan
  | (Rational(an, ad, ac), Rational(bn, bd, bc)) when ac == bc =>
    divRat(an, ad, bn, bd, Unit)
  | (Rational(an, ad, c), Rational(bn, bd, Unit)) =>
    divRat(an, ad, bn, bd, c)
  | (Rational(an, ad, Exp(aExp) as ac), Rational(bn, bd, Exp(bExp) as bc)) =>
    switch (divRat(an, ad, bn, bd, Unit), SafeOps.sub(aExp, bExp)) {
    | (Rational(n, d, _), Some(c)) => ratConstExp(n, d, c)
    | _ => Float(ratFloat(an, ad, ac) /. ratFloat(bn, bd, bc))
    }
  | (Rational(an, ad, Sqrt(aSqrt)), Rational(bn, bd, Sqrt(bSqrt)))
      when aSqrt > bSqrt && aSqrt mod bSqrt == 0 =>
    divRat(an, ad, bn, bd, Sqrt(aSqrt / bSqrt))
  | (a, b) => Float(toFloat(a) /. toFloat(b))
  };

let (==) = equal;