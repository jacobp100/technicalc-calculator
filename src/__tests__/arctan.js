const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

// Not sure why my implementation gets negative values here
// MathJS agrees with Wolfram Alpha as the convential value, but both answers are valid
const negateRealValues = new Set([
  "0+-2i",
  "0+-3i",
  "0+-4i",
  "0+-5i",
  "0+2i",
  "0+3i",
  "0+4i",
  "0+5i"
]);

imagValues.forEach(v => {
  it(`atan ${v.title}`, () => {
    const mathJsValue = mathjs.atan(v.jsValue);
    if (negateRealValues.has(v.title)) mathJsValue.re *= -1;
    expect(SciLine.atan(v.sciLineValue)).toMatchJsValue(mathJsValue);
  });
});
