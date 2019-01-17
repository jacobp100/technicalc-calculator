const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

trigValues.forEach(v => {
  it(`cos ${v.title}`, () => {
    expect(SciLine.cos(v.sciLineValue)).toMatchJsValue(Math.cos(v.jsValue));
  });
});
