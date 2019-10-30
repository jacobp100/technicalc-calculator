const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(imagValues)("acos(%s)", v => {
  const actual = SciLine.acos(v.sciLineValue);
  const expected = mathjs.acos(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
