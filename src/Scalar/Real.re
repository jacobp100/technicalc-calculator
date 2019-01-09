let rec _greatest_common_divisor = (a, b) =>
  switch (Int64.rem(a, b)) {
  | 0L => b
  | r => _greatest_common_divisor(b, r)
  };

let _float_is_int = a => floor(a) == a;

let (+) = Int64.add;
/* let (-) = Int64.sub; */
let ( * ) = Int64.mul;
let (/) = Int64.div;
let (mod) = Int64.rem;
let (~-) = Int64.neg;

type constant =
  | None
  | Pi
  | Exp(int64)
  | Sqrt(int64);

type rational = {
  numerator: int64,
  denominator: int64,
  constant,
};

type t =
  | Rational(rational)
  | Float(float)
  | Zero
  | NaN;

let float_of_constant = a =>
  switch (a) {
  | None => 1.0
  | Pi => 4.0 *. atan(1.0)
  | Exp(v) => exp(Int64.to_float(v))
  | Sqrt(v) => sqrt(Int64.to_float(v))
  };

let to_float = a =>
  switch (a) {
  | Rational(ar) =>
    Int64.to_float(ar.numerator)
    /. Int64.to_float(ar.denominator)
    *. float_of_constant(ar.constant)
  | Float(v) => v
  | Zero => 0.0
  | NaN => infinity
  };

let normalize = a =>
  switch (a) {
  | Rational({denominator: 0L})
  | NaN => NaN
  | Zero
  | Rational({numerator: 0L}) => Zero
  | Float(value) =>
    switch (classify_float(value)) {
    | FP_zero => Zero
    | FP_infinite
    | FP_nan => NaN
    | _ => Float(value)
    }
  | Rational(ar) =>
    let divisor =
      _greatest_common_divisor(Int64.abs(ar.numerator), ar.denominator);
    Rational({
      numerator: ar.numerator / divisor,
      denominator: ar.denominator / divisor,
      constant: ar.constant,
    });
  };

let of_int64 = (~denominator=1L, ~constant=None, numerator) =>
  normalize(Rational({numerator, denominator, constant}));

let of_float = (~constant=None, v) => {
  let magnitude = 1.e6;
  let int_max_f = Int64.to_float(Int64.max_int);
  let numerator_f = v *. magnitude;
  if (abs_float(numerator_f) < int_max_f && _float_is_int(numerator_f)) {
    let numerator = Int64.of_float(numerator_f);
    let denominator = Int64.of_float(magnitude);
    normalize(Rational({numerator, denominator, constant}));
  } else {
    normalize(Float(v *. float_of_constant(constant)));
  };
};

let zero = Zero;
let is_zero = (==)(Zero);

let nan = NaN;
let is_nan = (==)(NaN);

let pi = Rational({numerator: 1L, denominator: 1L, constant: Pi});
let e = Rational({numerator: 1L, denominator: 1L, constant: Exp(1L)});

let neg = a =>
  switch (a) {
  | Rational(ar) => Rational({...ar, numerator: - ar.numerator})
  | Float(v) => of_float(-. v)
  | Zero
  | NaN => a
  };

let add = (a, b) =>
  switch (a, b) {
  | (Rational(ar), Rational(br)) when ar.constant == br.constant =>
    let numerator =
      ar.numerator * br.denominator + ar.denominator * br.numerator;
    let denominator = ar.denominator * br.denominator;
    of_int64(numerator, ~denominator, ~constant=ar.constant);
  | (NaN, _)
  | (_, NaN) => NaN
  | (Zero, _) => b
  | (_, Zero) => a
  | (_, _) => of_float(to_float(a) +. to_float(b))
  };

let sub = (a, b) => add(a, neg(b));

let mul = (a, b) =>
  switch (a, b) {
  | (Rational(ar), Rational(br))
      when ar.constant == None || br.constant == None =>
    let constant = ar.constant != None ? ar.constant : br.constant;
    of_int64(
      ar.numerator * br.numerator,
      ~denominator=ar.denominator * br.denominator,
      ~constant,
    );
  | (
      Rational({numerator: an, denominator: ad, constant: Sqrt(ac)}),
      Rational({numerator: bn, denominator: bd, constant: Sqrt(bc)}),
    ) =>
    let numerator = an * bn;
    let denominator = ad * bd;
    ac == bc ?
      of_int64(numerator * ac, ~denominator) :
      of_int64(numerator, ~denominator, ~constant=Sqrt(ac * bc));
  | (NaN, _)
  | (_, NaN) => NaN
  | (Zero, _)
  | (_, Zero) => Zero
  | (_, _) => of_float(to_float(a) *. to_float(b))
  };

let div = (a, b) =>
  switch (a, b) {
  | (NaN, _)
  | (_, NaN | Zero) => NaN
  | (Zero, _) => Zero
  | (Rational(ar), Rational(br)) =>
    let bSign = to_float(b) >= 0.0 ? 1L : 0L;
    let numerator = br.denominator * ar.numerator * bSign;
    let denominator = Int64.abs(br.numerator * ar.denominator);
    switch (ar.constant, br.constant) {
    | (_, None) => of_int64(numerator, ~denominator, ~constant=ar.constant)
    | (Sqrt(x), Sqrt(y)) when x > y && x mod y == 0L =>
      of_int64(numerator, ~denominator, ~constant=Sqrt(x / y))
    | (None, Sqrt(x)) =>
      let denominator = denominator * x;
      of_int64(numerator, ~denominator, ~constant=Sqrt(x));
    | _ => of_float(to_float(a) /. to_float(b))
    };
  | _ => of_float(to_float(a) /. to_float(b))
  };

let _trig_period = a =>
  switch (a) {
  | Rational({numerator, denominator, constant: Pi as constant}) =>
    let divisor = 2L * denominator;
    let numerator = numerator mod divisor;
    /* Handle negative numerators */
    let numerator = (numerator + divisor) mod divisor;
    Rational({numerator, denominator, constant});
  | _ => a
  };

let sqrt = a =>
  switch (a) {
  | Rational({numerator: x, denominator: 1L, constant: None}) =>
    of_int64(~constant=Sqrt(x), 1L)
  | _ => of_float(sqrt(to_float(a)))
  };

let exp = a =>
  switch (a) {
  | Rational({numerator, denominator: 1L, constant: None}) =>
    of_int64(~constant=Exp(numerator), 1L)
  | _ => of_float(exp(to_float(a)))
  };

let sin = a =>
  switch (_trig_period(a)) {
  | Zero
  | Rational({numerator: 1L | 2L, denominator: 1L, constant: Pi}) =>
    of_int64(0L)
  | Rational({numerator: 1L | 5L, denominator: 2L, constant: Pi}) =>
    of_int64(1L)
  | Rational({numerator: 3L | 7L, denominator: 2L, constant: Pi}) =>
    of_int64(-1L)
  | Rational({numerator: 1L | 2L, denominator: 3L, constant: Pi}) =>
    of_int64(1L, ~denominator=2L, ~constant=Sqrt(3L))
  | Rational({numerator: 4L | 5L, denominator: 3L, constant: Pi}) =>
    of_int64(-1L, ~denominator=2L, ~constant=Sqrt(3L))
  | Rational({numerator: 1L | 3L, denominator: 4L, constant: Pi}) =>
    of_int64(1L, ~denominator=2L, ~constant=Sqrt(2L))
  | Rational({numerator: 5L | 7L, denominator: 4L, constant: Pi}) =>
    of_int64(-1L, ~denominator=2L, ~constant=Sqrt(2L))
  | Rational({numerator: 1L | 5L, denominator: 6L, constant: Pi}) =>
    of_int64(1L, ~denominator=2L)
  | Rational({numerator: 7L | 11L, denominator: 6L, constant: Pi}) =>
    of_int64(-1L, ~denominator=2L)
  | NaN => NaN
  | _ => of_float(sin(to_float(a)))
  };

let cos = a => sin(add(a, of_int64(1L, ~denominator=2L, ~constant=Pi)));

let tan = a =>
  switch (_trig_period(a)) {
  | Zero
  | Rational({numerator: 1L | 2L, denominator: 1L, constant: Pi}) =>
    of_int64(0L)
  | Rational({numerator: 1L | 5L, denominator: 4L, constant: Pi}) =>
    of_int64(1L)
  | Rational({numerator: 1L | 4L, denominator: 3L, constant: Pi}) =>
    of_int64(1L, ~constant=Sqrt(3L))
  | Rational({numerator: 1L | 7L, denominator: 6L, constant: Pi}) =>
    of_int64(1L, ~denominator=3L, ~constant=Sqrt(3L))
  | Rational({numerator: 2L | 5L, denominator: 3L, constant: Pi}) =>
    of_int64(-1L, ~constant=Sqrt(3L))
  | Rational({numerator: 3L | 7L, denominator: 4L, constant: Pi}) =>
    of_int64(-1L)
  | Rational({numerator: 5L | 11L, denominator: 6L, constant: Pi}) =>
    of_int64(-1L, ~denominator=3L, ~constant=Sqrt(3L))
  | Rational({numerator: 1L | 3L, denominator: 2L, constant: Pi})
  | NaN => NaN
  | _ => of_float(tan(to_float(a)))
  };

let pow = (a, b) =>
  switch (a, b) {
  | (_, Rational({numerator: 1L, denominator: 2L, constant: None})) =>
    sqrt(a)
  | (Rational({numerator: 1L, denominator: 1L, constant: Exp(1L)}), _) =>
    exp(b)
  | (
      Rational({numerator: 1L, denominator: 1L, constant: Sqrt(x)}),
      Rational({numerator: 2L, denominator: 1L, constant: None}),
    ) =>
    of_int64(x)
  | _ => of_float(to_float(a) ** to_float(b))
  };

let log = a =>
  switch (a) {
  | Rational({numerator: 1L, denominator: 1L, constant: Exp(x)}) =>
    of_int64(x)
  | _ => of_float(log(to_float(a)))
  };

let to_string = a =>
  switch (a) {
  | Rational(ar) =>
    let numerator_str =
      ar.numerator == 1L && ar.constant != None ?
        "" : Int64.to_string(ar.numerator);
    let constant_str =
      switch (ar.constant) {
      | None => ""
      | Pi => "\\pi"
      | Exp(v) => "e^{" ++ Int64.to_string(v) ++ "}"
      | Sqrt(v) => "\\sqrt{" ++ Int64.to_string(v) ++ "}"
      };
    let denominator =
      ar.denominator != 1L ? "/" ++ Int64.to_string(ar.denominator) : "";
    numerator_str ++ constant_str ++ denominator;
  | Float(v) => string_of_float(v)
  | NaN => "NaN"
  | Zero => "0"
  };
