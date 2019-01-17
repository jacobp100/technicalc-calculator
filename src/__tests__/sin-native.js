const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

trigValues.forEach(v => {
  it(`sin ${v.title}`, () => {
    expect(SciLine.sin(v.sciLineValue)).toMatchJsValue(Math.sin(v.jsValue));
  });
});
