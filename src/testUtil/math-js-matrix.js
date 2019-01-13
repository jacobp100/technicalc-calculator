const { range } = require("lodash");
const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value } = require(".");
const SciLine = require("../SciLine.bs");

const matrix2x2 = cartesian([
  [0, -1, 1, 5],
  [0, -1, 1, 5],
  [0, -1, 1, 5],
  [0, -1, 1, 5]
]).map(
  ([a, b, c, d]) =>
    new Value(
      mathjs.matrix([[a, b], [c, d]]),
      SciLine.matrix_of_elements(2, 2, [a, b, c, d].map(SciLine.of_float)),
      `[[${a}, ${b}], [${c}, ${d}]]`
    )
);
