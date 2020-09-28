open Formatting_Types;

let formatOperator = (op, format) =>
  format.mode == MathML ? "<mo>" ++ op ++ "</mo>" : op;

let formatVariable = (var, format) =>
  format.mode == MathML ? "<mi>" ++ var ++ "</mi>" : var;
