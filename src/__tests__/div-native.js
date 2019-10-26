const { toMatchJsValue } = require("../__test-util__");
const { binaryValues } = require("../__test-util__/math-native");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(binaryValues)("%s / %s", (a, b) => {
  const actual = SciLine.div(a.sciLineValue, b.sciLineValue);
  const expected = a.jsValue / b.jsValue;
  expect(actual).toMatchJsValue(expected);
});
