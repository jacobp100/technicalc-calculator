const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const SciLine = require("../Value.bs");
const ScilineTest = require("../ScilineTest.bs");

expect.extend({ toMatchJsValue });

const tanInfiniteValues = new Set([
  "1pi/2",
  "3pi/2",
  "5pi/2",
  "7pi/2",
  "9pi/2",
  "11pi/2",
  "-1pi/2",
  "-3pi/2",
  "-5pi/2",
  "-7pi/2",
  "-9pi/2",
  "-11pi/2"
]);

test.each(trigValues)("tan(%s)", v => {
  const actual = SciLine.tan(v.sciLineValue);
  if (tanInfiniteValues.has(v.title)) {
    expect(ScilineTest.toString(actual)).toBe("NaN");
  } else {
    const expected = Math.tan(v.jsValue);
    expect(actual).toMatchJsValue(expected);
  }
});
