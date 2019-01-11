const { toMatchJsValue } = require("../testUtil");
const { trigValues } = require("../testUtil/math-native");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

trigValues.forEach(v => {
  it(v.title, () => {
    expect(SciLine.cos(v.sciLineValue)).toMatchJsValue(Math.cos(v.jsValue));
  });
});
