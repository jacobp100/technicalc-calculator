open Real_Types;

/* Disable ops that can overflow */
let (+) = None;
let (-) = None;
let ( * ) = None;
let (~-) = None;

let _safeRat = (n, d, c) => {
  switch (SafeInt.toInt(n), SafeInt.toInt(d)) {
  | (Some(n), Some(d)) => Some(rat(n, d, c))
  | _ => None
  };
};

let inv = a => {
  switch (a) {
  | Rational(n, d, Unit) => Rational(d, n, Unit)
  | Rational(n, d, Exp(exp) as c) =>
    switch (SafeInt.negInt(exp)) {
    | Some(exp) => Rational(d, n, Exp(exp))
    | _ => Float(1. /. ratFloat(n, d, c))
    }
  | _ => Float(1. /. toFloat(a))
  };
};

let neg = a =>
  switch (a) {
  | Rational(n, d, c) =>
    switch (SafeInt.negInt(n)) {
    | Some(n) => Rational(n, d, c)
    | _ => Float(-. ratFloat(n, d, c))
    }
  | Float(f) => Float(-. f)
  };

let abs = a =>
  switch (a) {
  | Rational(n, d, c) =>
    switch (SafeInt.absInt(n)) {
    | Some(n) => Rational(n, d, c)
    | _ => Float(abs_float(ratFloat(n, d, c)))
    }
  | Float(f) => Float(abs_float(f))
  };

let _ofFloatInt = f => {
  let intF = int_of_float(f);
  if (float_of_int(intF) == f) {
    Rational(intF, 1, Unit);
  } else {
    Float(f);
  };
};

let round = a => floor(toFloat(a) +. 0.5)->_ofFloatInt;
let floor = a => toFloat(a)->floor->_ofFloatInt;
let ceil = a => toFloat(a)->ceil->_ofFloatInt;

let add = (a, b) => {
  let rat =
    switch (a, b) {
    | (Rational(an, ad, c), Rational(bn, bd, bc)) when c == bc =>
      open SafeInt;
      let ad = fromInt(ad);
      let bd = fromInt(bd);
      let n = fromInt(an) * bd + fromInt(bn) * ad;
      let d = ad * bd;
      _safeRat(n, d, c);
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => Float(toFloat(a) +. toFloat(b))
  };
};

let sub = (a, b) => {
  let rat =
    switch (a, b) {
    | (Rational(an, ad, c), Rational(bn, bd, bc)) when c == bc =>
      open SafeInt;
      let ad = fromInt(ad);
      let bd = fromInt(bd);
      let n = fromInt(an) * bd - fromInt(bn) * ad;
      let d = ad * bd;
      _safeRat(n, d, c);
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => Float(toFloat(a) -. toFloat(b))
  };
};

let _mulRat = (an, ad, bn, bd, c) => {
  open SafeInt;
  let n = fromInt(an) * fromInt(bn);
  let d = fromInt(ad) * fromInt(bd);
  _safeRat(n, d, c);
};

let mul = (a, b) => {
  let rat =
    switch (a, b) {
    | (Rational(an, ad, c), Rational(bn, bd, Unit))
    | (Rational(an, ad, Unit), Rational(bn, bd, c)) =>
      _mulRat(an, ad, bn, bd, c)
    | (Rational(an, ad, Exp(aExp)), Rational(bn, bd, Exp(bExp))) =>
      switch (SafeInt.addInt(aExp, bExp)) {
      | Some(exp) => _mulRat(an, ad, bn, bd, Exp(exp))
      | _ => None
      }
    | (Rational(an, ad, Sqrt(aSqrt)), Rational(bn, bd, Sqrt(bSqrt))) =>
      switch (SafeInt.mulInt(aSqrt, bSqrt)) {
      | Some(sqrt) => _mulRat(an, ad, bn, bd, Sqrt(sqrt))
      | _ => None
      }
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => Float(toFloat(a) *. toFloat(b))
  };
};

let _divRat = (an, ad, bn, bd, c) => {
  open SafeInt;
  let n = fromInt(an) * fromInt(bd);
  let d = fromInt(ad) * fromInt(bn);
  _safeRat(n, d, c);
};

let div = (a, b) => {
  let rat =
    switch (a, b) {
    | (_, Rational(0, _, _)) => Some(nan)
    | (Rational(an, ad, ac), Rational(bn, bd, bc)) when ac == bc =>
      _divRat(an, ad, bn, bd, Unit)
    | (Rational(an, ad, c), Rational(bn, bd, Unit)) =>
      _divRat(an, ad, bn, bd, c)
    | (Rational(an, ad, Exp(aExp)), Rational(bn, bd, Exp(bExp))) =>
      switch (SafeInt.subInt(aExp, bExp)) {
      | Some(exp) => _divRat(an, ad, bn, bd, Exp(exp))
      | _ => None
      }
    | (Rational(an, ad, Sqrt(aSqrt)), Rational(bn, bd, Sqrt(bSqrt)))
        when aSqrt > bSqrt && aSqrt mod bSqrt == 0 =>
      switch (SafeInt.divInt(aSqrt, bSqrt)) {
      | Some(sqrt) => _divRat(an, ad, bn, bd, Sqrt(sqrt))
      | _ => None
      }
    | _ => None
    };
  switch (rat) {
  | Some(rat) => rat
  | None => Float(toFloat(a) /. toFloat(b))
  };
};

let powInt = (a, b) => {
  let baseRat =
    switch (a, Pervasives.abs(b)) {
    | (Rational(n, d, Unit), b) =>
      open SafeInt;
      let n = (fromInt(n) ** fromInt(b))->toInt;
      let d = (fromInt(d) ** fromInt(b))->toInt;
      switch (n, d) {
      | (Some(n), Some(d)) => Some(rat(n, d, Unit))
      | _ => None
      };
    | (_, 2) => Some(mul(a, a))
    | _ => None
    };
  switch (baseRat) {
  | Some(baseRat) => b >= 0 ? baseRat : inv(baseRat)
  | _ => Float(toFloat(a) ** float_of_int(b))
  };
};

let (~-) = neg;
let (+) = add;
let (-) = sub;
let ( * ) = mul;
let (/) = div;
let cmp = (a, b) => compare(toFloat(a), toFloat(b));
let (==) = equal;
let (>) = (a, b) => toFloat(a) > toFloat(b);
let (<) = (a, b) => toFloat(a) < toFloat(b);