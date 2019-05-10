const { toMatchJsValue } = require("../__test-util__");
const { binaryValues } = require("../__test-util__/math-native");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

binaryValues.forEach(([lhs, rhs]) => {
  it(`${lhs.title} / ${rhs.title}`, () => {
    expect(SciLine.div(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue / rhs.jsValue
    );
  });
});
