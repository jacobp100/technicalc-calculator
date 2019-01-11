const { toMatchJsValue } = require("../testUtil");
const { trigValues } = require("../testUtil/math-native");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

trigValues.forEach(v => {
  it(v.title, () => {
    expect(SciLine.sin(v.sciLineValue)).toMatchJsValue(Math.sin(v.jsValue));
  });
});
