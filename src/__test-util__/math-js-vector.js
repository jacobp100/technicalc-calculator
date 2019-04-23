const cartesian = require("cartesian");
const { Value } = require(".");
const SciLine = require("../SciLine.bs");

const elements = [0, 1, -1, 5];

const vector2 = cartesian([elements, elements]).map(
  ([a, b]) =>
    new Value(
      [a, b],
      SciLine.vector2(...[a, b].map(SciLine.ofFloat)),
      `[${a}, ${b}]`
    )
);
module.exports.vector2 = vector2;

const vector3 = cartesian([elements, elements, elements]).map(
  ([a, b, c]) =>
    new Value(
      [a, b, c],
      SciLine.vector3(...[a, b, c].map(SciLine.ofFloat)),
      `[${a}, ${b}, ${c}]`
    )
);
module.exports.vector3 = vector3;
