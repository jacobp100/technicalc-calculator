const { range } = require("lodash");
const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value, fractionsTo12 } = require(".");
const SciLine = require("../SciLine.bs");

const trigValues = cartesian([fractionsTo12, fractionsTo12]).map(
  ([[numRe, denomRe], [numIm, denomIm]]) =>
    new Value(
      mathjs.complex((Math.PI * numRe) / denomRe, (Math.PI * numIm) / denomIm),
      SciLine.mul(
        SciLine.add(
          SciLine.div(SciLine.of_float(numRe), SciLine.of_float(denomRe)),
          SciLine.mul(
            SciLine.div(SciLine.of_float(numIm), SciLine.of_float(denomIm)),
            SciLine.i
          )
        ),
        SciLine.pi
      ),
      `(${numRe}/${denomRe}+${numIm}i/${denomIm})pi`
    )
);

module.exports.trigValues = trigValues;

const imagValues = cartesian([
  [0.5, ...range(-3, 3 + 1)],
  [0.5, ...range(-3, 3 + 1)]
]).map(([re, im]) => Value.complex(re, im));
module.exports.imagValues = imagValues;
