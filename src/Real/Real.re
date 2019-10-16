external hi: int64 => nativeint = "%field0";
external lo: int64 => nativeint = "%field1";

module I64Ops = {
  include Int64;
  let (+) = add;
  let (-) = sub;
  let ( * ) = mul;
  let (/) = div;
  let (~-) = neg;
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

let fitsInt32 = n => I64Ops.(compare(of_int(to_int(n)), n) === 0);

let intSafe = n => fitsInt32(n) ? Some(I64Ops.to_int(n)) : None;

let negSafe = a => I64Ops.(- of_int(a))->intSafe;
let absSafe = a => I64Ops.(of_int(a)->abs)->intSafe;
let addIntSafe = (a, b) => I64Ops.(of_int(a) + of_int(b))->intSafe;
let subIntSafe = (a, b) => I64Ops.(of_int(a) - of_int(b))->intSafe;
let mulIntSafe = (a, b) => I64Ops.(of_int(a) * of_int(b))->intSafe;

let rec gcd64 = (a, b) =>
  if (b == 0L) {
    a;
  } else {
    gcd64(b, Int64.rem(a, b));
  };

let rat64 = (n, d, c) => {
  open I64Ops;
  let gcd = gcd64(n, d);
  let n = n / gcd;
  let d = d / gcd;
  if (fitsInt32(n) && fitsInt32(d)) {
    Rational(to_int(n), to_int(d), c);
  } else {
    Float(to_float(n) /. to_float(d));
  };
};

let addRat = (an, ad, bn, bd, c) => {
  open I64Ops;
  let ad = of_int(ad);
  let bd = of_int(bd);
  let n = of_int(an) * bd + of_int(bn) * ad;
  let d = ad * bd;
  rat64(n, d, c);
};

let transformRationalSafe = (a, f) =>
  switch (a) {
  | Rational(n, d, c) =>
    switch (f(n)) {
    | Some(n) => Rational(n, d, c)
    | _ => Float(-. ratFloat(n, d, c))
    }
  | Float(f) => Float(-. f)
  };

let neg = transformRationalSafe(_, negSafe);
let abs = transformRationalSafe(_, absSafe);

let add = (a, b) =>
  switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, bc)) when c == bc =>
    addRat(an, ad, bn, bd, c)
  | (Rational(an, ad, ac), Rational(bn, bd, bc)) =>
    Float(ratFloat(an, ad, ac) +. ratFloat(bn, bd, bc))
  | (Rational(n, d, c), Float(f))
  | (Float(f), Rational(n, d, c)) => Float(ratFloat(n, d, c) +. f)
  | (Float(af), Float(bf)) => Float(af +. bf)
  };

let sub = (a, b) => add(a, neg(b));

let mulRat = (an, ad, bn, bd, c) => {
  open I64Ops;
  let n = of_int(an) * of_int(bn);
  let d = of_int(ad) * of_int(bd);
  rat64(n, d, c);
};

let simplifyExpRat = (n, d, c) =>
  switch (Real_Constant.simplifyExp(c)) {
  | `None => Rational(n, d, Exp(c))
  | `Factor(n', c) => mulRat(n, d, n', 1, c)
  };

let simplifySqrtRat = (n, d, c) =>
  switch (Real_Constant.simplifySqrt(c)) {
  | `None => Rational(n, d, Sqrt(c))
  | `Zero => Rational(0, 1, Unit)
  | `Factor(n', c) => mulRat(n, d, n', 1, c)
  };

let mul = (a, b) =>
  switch (a, b) {
  | (Rational(an, ad, c), Rational(bn, bd, Unit))
  | (Rational(an, ad, Unit), Rational(bn, bd, c)) =>
    mulRat(an, ad, bn, bd, c)
  | (Rational(an, ad, Exp(aExp) as ac), Rational(bn, bd, Exp(bExp) as bc)) =>
    switch (mulRat(an, ad, bn, bd, Unit), addIntSafe(aExp, bExp)) {
    | (Rational(n, d, _), Some(c)) => simplifyExpRat(n, d, c)
    | _ => Float(ratFloat(an, ad, ac) *. ratFloat(bn, bd, bc))
    }
  | (Rational(an, ad, Sqrt(aS) as ac), Rational(bn, bd, Sqrt(bS) as bc)) =>
    switch (mulRat(an, ad, bn, bd, Unit), mulIntSafe(aS, bS)) {
    | (Rational(n, d, _), Some(c)) => simplifySqrtRat(n, d, c)
    | _ => Float(ratFloat(an, ad, ac) *. ratFloat(bn, bd, bc))
    }
  | (Rational(an, ad, ac), Rational(bn, bd, bc)) =>
    Float(ratFloat(an, ad, ac) *. ratFloat(bn, bd, bc))
  | (Rational(n, d, c), Float(f))
  | (Float(f), Rational(n, d, c)) => Float(ratFloat(n, d, c) *. f)
  | (Float(af), Float(bf)) => Float(af *. bf)
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
    switch (mulRat(an, ad, bn, bd, Unit), subIntSafe(aExp, bExp)) {
    | (Rational(n, d, _), Some(c)) => simplifyExpRat(n, d, c)
    | _ => Float(ratFloat(an, ad, ac) /. ratFloat(bn, bd, bc))
    }
  | (Rational(an, ad, Sqrt(aSqrt)), Rational(bn, bd, Sqrt(bSqrt)))
      when aSqrt > bSqrt && aSqrt mod bSqrt == 0 =>
    divRat(an, ad, bn, bd, Sqrt(aSqrt / bSqrt))
  | (Rational(an, ad, ac), Rational(bn, bd, bc)) =>
    Float(ratFloat(an, ad, ac) /. ratFloat(bn, bd, bc))
  | (Rational(n, d, c), Float(f))
  | (Float(f), Rational(n, d, c)) => Float(ratFloat(n, d, c) /. f)
  | (Float(af), Float(bf)) => Float(af /. bf)
  };

let (==) = equal;