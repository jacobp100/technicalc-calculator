include Types.Scalar;

let ofInt: (~denominator: int=?, ~constant: Constant.t=?, int) => t;
let ofFloat: (~constant: Constant.t=?, float) => t;
let ofStringBase: (~constant: Constant.t=?, int, string) => t;
let ofString: (~constant: Constant.t=?, string) => t;

let toFloat: t => float;
let toQ: t => option(Q.t);
let toZ: t => option(Z.t);

let isInt: t => bool;
let isNegative: t => bool;
