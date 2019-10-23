type t;

external fromInt: int => t = "%identity";
[@bs.module "./SafeInt"]
[@bs.module "./SafeInt"] external toInt: t => option(int) = "toInt";
[@bs.module "./SafeInt"] external abs: t => t = "abs";
[@bs.module "./SafeInt"] external neg: t => t = "neg";
[@bs.module "./SafeInt"] external add: (t, t) => t = "add";
[@bs.module "./SafeInt"] external sub: (t, t) => t = "sub";
[@bs.module "./SafeInt"] external mul: (t, t) => t = "mul";
[@bs.module "./SafeInt"] external div: (t, t) => t = "div";
[@bs.module "./SafeInt"] external pow: (t, t) => t = "pow";
[@bs.module "./SafeInt"] external rem: (t, t) => t = "mod";
[@bs.module "./SafeInt"] external (~-): t => t = "neg";
[@bs.module "./SafeInt"] external (+): (t, t) => t = "add";
[@bs.module "./SafeInt"] external (-): (t, t) => t = "sub";
[@bs.module "./SafeInt"] external ( * ): (t, t) => t = "mul";
[@bs.module "./SafeInt"] external (/): (t, t) => t = "div";
[@bs.module "./SafeInt"] external ( ** ): (t, t) => t = "pow";
[@bs.module "./SafeInt"] external (mod): (t, t) => t = "mod";

let negInt = a => fromInt(a)->neg->toInt;
let absInt = a => fromInt(a)->abs->toInt;
let addInt = (a, b) => (fromInt(a) + fromInt(b))->toInt;
let subInt = (a, b) => (fromInt(a) - fromInt(b))->toInt;
let mulInt = (a, b) => (fromInt(a) * fromInt(b))->toInt;
let divInt = (a, b) => (fromInt(a) / fromInt(b))->toInt;
let modInt = (a, b) => (fromInt(a) mod fromInt(b))->toInt;