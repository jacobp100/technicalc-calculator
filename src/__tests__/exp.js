const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("exp(%s)", v => {
  const actual = SciLine.exp(v.sciLineValue);
  const expected = mathjs.exp(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
