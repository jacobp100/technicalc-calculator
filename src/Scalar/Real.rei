include Types.Scalar;

type constant;

let pi: t;

let of_int64: (~denominator: int64=?, ~constant: constant=?, int64) => t;
let of_float: (~constant: constant=?, float) => t;

let float_of_real: t => float;
let to_string: t => string;
