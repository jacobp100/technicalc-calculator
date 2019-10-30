const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../Value.bs");
const ScilineTest = require("../ScilineTest.bs");

expect.extend({ toMatchJsMatrix });

describe("2x2", () => {
  test.each(cartesian([matrix2x2, matrix2x2]))("%s + %s", (a, b) => {
    const actualValue = SciLine.add(a.sciLineValue, b.sciLineValue);
    const expectedValue = mathjs.add(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("3x3", () => {
  test.each(cartesian([matrix3x3, matrix3x3]))("%s + %s", (a, b) => {
    const actualValue = SciLine.add(a.sciLineValue, b.sciLineValue);
    const expectedValue = mathjs.add(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("Type checking", () => {
  it("Cannot add incompatible sizes", () => {
    const value = SciLine.add(
      matrix2x2[0].sciLineValue,
      matrix3x3[0].sciLineValue
    );
    expect(ScilineTest.toString(value)).toBe("NaN");
  });
});
