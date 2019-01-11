const cartesian = require("cartesian");
const { Value, range12, fractionsTo12 } = require("../testUtil");
const SciLine = require("../SciLine.bs");

const range12Values = range12.map(Value.float);
const range12NegativeValues = range12.map(x => Value.float(-x));

const trigPositiveValues = fractionsTo12.map(
  ([num, denom]) =>
    new Value(
      (Math.PI * num) / denom,
      SciLine.mul(
        SciLine.div(SciLine.of_float(num), SciLine.of_float(denom)),
        SciLine.pi
      ),
      `${num}pi/${denom}`
    )
);

const trigNegativeValues = fractionsTo12.map(
  ([num, denom]) =>
    new Value(
      -(Math.PI * num) / denom,
      SciLine.mul(
        SciLine.div(SciLine.of_float(-num), SciLine.of_float(denom)),
        SciLine.pi
      ),
      `${-num}pi/${denom}`
    )
);

const positiveValues = [...range12Values, ...trigPositiveValues];

const values = [...positiveValues, ...range12NegativeValues];

const binaryValues = cartesian([values, values]);
module.exports.binaryValues = binaryValues;

const positiveBinaryValues = cartesian([positiveValues, positiveValues]);
module.exports.positiveBinaryValues = positiveBinaryValues;

const trigValues = [...trigPositiveValues, ...trigNegativeValues];
module.exports.trigValues = trigValues;
