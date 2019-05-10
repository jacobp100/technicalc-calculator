const { range } = require("lodash");
const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value, toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsMatrix });

const constantValues = range(-2, 2 + 1).map(Value.float);

describe("2x2", () => {
  cartesian([matrix2x2, constantValues]).forEach(([a, b]) => {
    it(`${a.title} * ${b.title}`, () => {
      const mathJsValue = mathjs.multiply(a.jsValue, b.jsValue);
      expect(SciLine.mul(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});

describe("3x3", () => {
  cartesian([matrix3x3, constantValues]).forEach(([a, b]) => {
    it(`${a.title} * ${b.title}`, () => {
      const mathJsValue = mathjs.multiply(a.jsValue, b.jsValue);
      expect(SciLine.mul(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});
