include Types.Scalar;

let of_int: (~denominator: int=?, ~constant: Constant.t=?, int) => t;
let of_float: (~constant: Constant.t=?, float) => t;
let of_string_base: (~constant: Constant.t=?, int, string) => t;
let of_string: (~constant: Constant.t=?, string) => t;

let to_float: t => float;
let to_q: t => option(Q.t);
let to_z: t => option(Z.t);

let is_int: t => bool;
let is_negative: t => bool;
