const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value } = require(".");
const Types = require("../Types.bs");
const SciLine = require("../Value.bs");

const matrix2x2 = cartesian([
  [0, -1, 1, 5],
  [0, -1, 1],
  [0, 1, 5],
  [0, -1, 1]
]).map(
  ([a, b, c, d]) =>
    new Value(
      mathjs.matrix([[a, b], [c, d]]),
      Types.matrix2(...[a, b, c, d].map(SciLine.ofFloat)),
      `[[${a}, ${b}], [${c}, ${d}]]`
    )
);
module.exports.matrix2x2 = matrix2x2;

const matrix3x3 = cartesian([
  [0, -1],
  [0, 1],
  [0, 5],
  [0, 1],
  [0, -1],
  [0],
  [0, 5],
  [0, 1],
  [0]
]).map(
  ([a, b, c, d, e, f, g, h, i]) =>
    new Value(
      mathjs.matrix([[a, b, c], [d, e, f], [g, h, i]]),
      Types.matrix3(...[a, b, c, d, e, f, g, h, i].map(SciLine.ofFloat)),
      `[[${a}, ${b}, ${c}], [${d}, ${e}, ${f}], [${g}, ${h}, ${i}]]`
    )
);
module.exports.matrix3x3 = matrix3x3;
