const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { vector2, vector3 } = require("../__test-util__/math-js-vector");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

describe("2", () => {
  cartesian([vector2, vector2]).forEach(([a, b]) => {
    it(`${a.title} . ${b.title}`, () => {
      expect(SciLine.dot(a.sciLineValue, b.sciLineValue)).toMatchJsValue(
        mathjs.dot(a.jsValue, b.jsValue)
      );
    });
  });
});

describe("3", () => {
  cartesian([vector3, vector3]).forEach(([a, b]) => {
    it(`${a.title} . ${b.title}`, () => {
      expect(SciLine.dot(a.sciLineValue, b.sciLineValue)).toMatchJsValue(
        mathjs.dot(a.jsValue, b.jsValue)
      );
    });
  });
});

describe("Type checking", () => {
  it("Cannot dot incompatible sizes", () => {
    const value = SciLine.resolve(
      SciLine.dot(vector3[0].sciLineValue, vector2[0].sciLineValue)
    );
    expect(SciLine.toString(value)).toBe("NaN");
  });
});
