const { toMatchJsValue } = require("../__test-util__");
const { positiveBinaryValues } = require("../__test-util__/math-native");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

const mathematicallyAccuratePow = (a, b) => (a === 0 && b === 0 ? NaN : a ** b);

positiveBinaryValues.forEach(([lhs, rhs]) => {
  it(`${lhs.title} ** ${rhs.title}`, () => {
    expect(SciLine.pow(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      mathematicallyAccuratePow(lhs.jsValue, rhs.jsValue)
    );
  });
});
