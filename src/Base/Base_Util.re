let percentToNumerical = {
  open Types;
  let _100 = real(Q.of_int(100));
  x => Base_Operators.div(valueOfScalar(x), _100);
};
