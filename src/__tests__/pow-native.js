const { toMatchJsValue } = require("../__test-util__");
const { positiveBinaryValues } = require("../__test-util__/math-native");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

const mathematicallyAccuratePow = (a, b) => (a === 0 && b === 0 ? NaN : a ** b);

test.each(positiveBinaryValues)("(%s) ** (%s)", (a, b) => {
  const actual = SciLine.pow(a.sciLineValue, b.sciLineValue);
  const expected = mathematicallyAccuratePow(a.jsValue, b.jsValue);
  expect(actual).toMatchJsValue(expected);
});
