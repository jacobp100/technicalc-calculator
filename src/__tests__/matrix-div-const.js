const { range } = require("lodash");
const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value, toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsMatrix });

const constantValues = range(-2, 2 + 1)
  .filter(x => x !== 0)
  .map(Value.float);

describe("2x2", () => {
  test.each(cartesian([matrix2x2, constantValues]))("%s / %s", (a, b) => {
    const actualValue = SciLine.div(a.sciLineValue, b.sciLineValue);
    const expectedValue = mathjs.divide(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("3x3", () => {
  test.each(cartesian([matrix3x3, constantValues]))("%s / %s", (a, b) => {
    const actualValue = SciLine.div(a.sciLineValue, b.sciLineValue);
    const expectedValue = mathjs.divide(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});
