const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const { vector3 } = require("../__test-util__/math-js-vector");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsMatrix });

describe("2x2", () => {
  cartesian([matrix2x2, matrix2x2]).forEach(([a, b]) => {
    it(`${a.title} * ${b.title}`, () => {
      const mathJsValue = mathjs.multiply(a.jsValue, b.jsValue);
      expect(SciLine.mul(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});

describe("3x3", () => {
  cartesian([matrix3x3, matrix3x3]).forEach(([a, b]) => {
    it(`${a.title} * ${b.title}`, () => {
      const mathJsValue = mathjs.multiply(a.jsValue, b.jsValue);
      expect(SciLine.mul(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});

describe("Cross product", () => {
  cartesian([vector3, vector3]).forEach(([a, b]) => {
    it(`${a.title} x ${b.title}`, () => {
      const [i, j, k] = mathjs.cross(a.jsValue, b.jsValue);
      const mathJsValue = mathjs.matrix([[i], [j], [k]]);
      expect(SciLine.mul(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});

describe("Type checking", () => {
  it("Cannot multiply incompatible sizes", () => {
    const value = SciLine.resolve(
      SciLine.mul(matrix2x2[0].sciLineValue, matrix3x3[0].sciLineValue)
    );
    expect(SciLine.is_nan(value)).toBeTruthy();
  });
});
