const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("acosh(%s)", v => {
  const actual = SciLine.acosh(v.sciLineValue);
  const expected = mathjs.acosh(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
