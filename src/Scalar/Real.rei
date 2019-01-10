include Types.Scalar;

type constant;

let of_int: (~denominator: int=?, ~constant: constant=?, int) => t;
let of_float: (~constant: constant=?, float) => t;

let to_float: t => float;
let to_string: t => string;
