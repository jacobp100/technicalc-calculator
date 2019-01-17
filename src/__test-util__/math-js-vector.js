const cartesian = require("cartesian");
const { Value } = require(".");
const SciLine = require("../SciLine.bs");

const elements = [0, 1, -1, 5];

const vector2 = cartesian([elements, elements]).map(
  ([a, b]) =>
    new Value(
      [a, b],
      SciLine.matrix_of_elements(2, 1, [a, b].map(SciLine.of_float)),
      `[${a}, ${b}]`
    )
);
module.exports.vector2 = vector2;

const vector3 = cartesian([elements, elements, elements]).map(
  ([a, b, c]) =>
    new Value(
      [a, b, c],
      SciLine.matrix_of_elements(3, 1, [a, b, c].map(SciLine.of_float)),
      `[${a}, ${b}, ${c}]`
    )
);
module.exports.vector3 = vector3;
