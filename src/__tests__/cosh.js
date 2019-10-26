const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("cosh(%s)", v => {
  const actual = SciLine.cosh(v.sciLineValue);
  const expected = mathjs.cosh(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
