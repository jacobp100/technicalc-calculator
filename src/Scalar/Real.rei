include Types.Scalar;

let of_int: (~denominator: int=?, ~constant: Constant.t=?, int) => t;
let of_float: (~constant: Constant.t=?, float) => t;

let to_float: t => float;
let to_string: t => string;
