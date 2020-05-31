const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

const values = cartesian([imagValues, imagValues]);

test.each(values)("%s / %s", (a, b) => {
  const actual = TechniCalc.div(a.techniCalcValue, b.techniCalcValue);
  const expected = mathjs.divide(a.jsValue, b.jsValue);
  expect(actual).toMatchJsValue(expected);
});
