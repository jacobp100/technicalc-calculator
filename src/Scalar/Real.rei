include Types.Scalar;

type constant;

let of_int64: (~denominator: int64=?, ~constant: constant=?, int64) => t;

let float_of_real: t => float;
