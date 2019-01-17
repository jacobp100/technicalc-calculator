const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const SciLine = require("../SciLine.bs");

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

trigValues.forEach(v => {
  it(`tan ${v.title}`, () => {
    const sciLineValue = SciLine.tan(v.sciLineValue);
    if (tanInfiniteValues.has(v.title)) {
      expect(SciLine.to_string(SciLine.resolve(sciLineValue))).toBe(
        "Math Error"
      );
    } else {
      expect(sciLineValue).toMatchJsValue(Math.tan(v.jsValue));
    }
  });
});
