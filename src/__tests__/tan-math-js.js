const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(trigValues)("tan(%s)", v => {
  const actual = SciLine.tan(v.sciLineValue);
  const expected = mathjs.tan(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
