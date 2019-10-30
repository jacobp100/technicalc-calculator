const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("asinh(%s)", v => {
  const actual = SciLine.asinh(v.sciLineValue);
  const expected = mathjs.asinh(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
