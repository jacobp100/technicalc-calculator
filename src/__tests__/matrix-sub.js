const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsMatrix });

describe("2x2", () => {
  cartesian([matrix2x2, matrix2x2]).forEach(([a, b]) => {
    it(`${a.title} + ${b.title}`, () => {
      const mathJsValue = mathjs.subtract(a.jsValue, b.jsValue);
      expect(SciLine.sub(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});

describe("3x3", () => {
  cartesian([matrix3x3, matrix3x3]).forEach(([a, b]) => {
    it(`${a.title} + ${b.title}`, () => {
      const mathJsValue = mathjs.subtract(a.jsValue, b.jsValue);
      expect(SciLine.sub(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});

describe("Type checking", () => {
  it("Cannot sub incompatible sizes", () => {
    const value = SciLine.resolve(
      SciLine.sub(matrix2x2[0].sciLineValue, matrix3x3[0].sciLineValue)
    );
    expect(SciLine.to_string(value)).toBe("Math Error");
  });
});
