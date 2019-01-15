const cartesian = require("cartesian");
const { Value } = require(".");
const SciLine = require("../SciLine.bs");

const vector3 = cartesian([[0, 1, -1, 5], [0, 1, -1, 5], [0, 1, -1, 5]]).map(
  ([a, b, c]) =>
    new Value(
      [a, b, c],
      SciLine.matrix_of_elements(3, 1, [a, b, c].map(SciLine.of_float)),
      `[${a}, ${b}, ${c}]`
    )
);
module.exports.vector3 = vector3;
