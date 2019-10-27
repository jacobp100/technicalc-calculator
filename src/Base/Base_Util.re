let percentToNumerical = {
  open Types;
  let _100 = ofInt(100);
  x => Base_Operators.div(valueOfScalar(x), _100);
};