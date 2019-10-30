const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

const values = cartesian([imagValues, imagValues]);

test.each(values)("%s + %s", (a, b) => {
  const actual = SciLine.add(a.sciLineValue, b.sciLineValue);
  const expected = mathjs.add(a.jsValue, b.jsValue);
  expect(actual).toMatchJsValue(expected);
});
