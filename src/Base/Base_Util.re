let percentToNumerical = {
  open Types;
  let _100 = `Real(Real.rational(100, 1, Unit));
  x => Base_Operators.div(valueOfScalar(x), _100);
};