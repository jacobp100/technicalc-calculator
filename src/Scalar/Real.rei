include Types.Scalar;

let of_int: (~denominator: int=?, ~constant: Constant.t=?, int) => t;
let of_float: (~constant: Constant.t=?, float) => t;
let of_string: (~constant: Constant.t=?, string) => t;

let to_float: t => float;

let is_integer: t => bool;
