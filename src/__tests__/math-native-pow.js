const { toMatchJsValue } = require("../testUtil");
const { positiveBinaryValues } = require("../testUtil/math-native");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

positiveBinaryValues.forEach(([lhs, rhs]) => {
  it(`${lhs.title} ** ${rhs.title}`, () => {
    expect(SciLine.pow(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue ** rhs.jsValue
    );
  });
});
