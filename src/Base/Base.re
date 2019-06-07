include Base_Util;

let equal = Base_Comparison.equal;
let neg = Base_Functions.neg;
let abs = Base_Magnitude.abs;
let floor = Base_Functions.floor;
let ceil = Base_Functions.ceil;
let round = Base_Functions.round;
let add = Base_Operators.add;
let sub = Base_Operators.sub;
let mul = Base_Operators.mul;
let div = Base_Operators.div;
let pow = Base_Powers.pow;
let sqrt = Base_Powers.sqrt;
let exp = Base_Exponentiation.exp;
let log = Base_Exponentiation.log;
let dot = Base_Dot.dot;

let (~-) = neg;
let (+) = add;
let (-) = sub;
let ( * ) = mul;
let (/) = div;
let ( ** ) = pow;
let (==) = equal;
