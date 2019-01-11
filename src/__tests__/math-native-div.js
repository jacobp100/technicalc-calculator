const { toMatchJsValue } = require("../testUtil");
const { binaryValues } = require("../testUtil/math-native");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

binaryValues.forEach(([lhs, rhs]) => {
  it(`${lhs.title} / ${rhs.title}`, () => {
    expect(SciLine.div(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue / rhs.jsValue
    );
  });
});
