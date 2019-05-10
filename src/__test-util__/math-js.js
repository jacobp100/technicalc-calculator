const { range } = require("lodash");
const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value, fractionsTo12 } = require(".");
const SciLineTest = require("../SciLineTest.bs");
const SciLine = require("../Value.bs");

const trigValues = cartesian([fractionsTo12, fractionsTo12]).map(
  ([[numRe, denomRe], [numIm, denomIm]]) =>
    new Value(
      mathjs.complex((Math.PI * numRe) / denomRe, (Math.PI * numIm) / denomIm),
      SciLine.mul(
        SciLine.add(
          SciLine.div(SciLine.ofFloat(numRe), SciLine.ofFloat(denomRe)),
          SciLine.mul(
            SciLine.div(SciLine.ofFloat(numIm), SciLine.ofFloat(denomIm)),
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
  [0.5, 0.25, 0.75, ...range(-5, 5 + 1)],
  [0.5, 0.25, 0.75, ...range(-5, 5 + 1)]
]).map(([re, im]) => Value.complex(re, im));
module.exports.imagValues = imagValues;
