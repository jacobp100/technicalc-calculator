const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("asin(%s)", v => {
  const actual = SciLine.asin(v.sciLineValue);
  const expected = mathjs.asin(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
