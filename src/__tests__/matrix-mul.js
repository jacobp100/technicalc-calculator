const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsMatrix } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const TechniCalc = require("../Value.bs");
const TechniCalcTest = require("../ValueTestUtil.bs");

expect.extend({ toMatchJsMatrix });

describe("2x2", () => {
  test.each(cartesian([matrix2x2, matrix2x2]))("%s * %s", (a, b) => {
    const actualValue = TechniCalc.mul(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.multiply(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("3x3", () => {
  test.each(cartesian([matrix3x3, matrix3x3]))("%s * %s", (a, b) => {
    const actualValue = TechniCalc.mul(a.techniCalcValue, b.techniCalcValue);
    const expectedValue = mathjs.multiply(a.jsValue, b.jsValue);
    expect(actualValue).toMatchJsMatrix(expectedValue);
  });
});

describe("Type checking", () => {
  it("Cannot multiply incompatible sizes", () => {
    const value = TechniCalc.mul(
      matrix2x2[0].techniCalcValue,
      matrix3x3[0].techniCalcValue
    );
    expect(TechniCalcTest.toString(value)).toBe("NaN");
  });
});
