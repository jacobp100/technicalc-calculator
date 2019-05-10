const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

matrix2x2.forEach(v => {
  it(`abs ${v.title}`, () => {
    const mathJsValue = mathjs.det(v.jsValue);
    expect(SciLine.abs(v.sciLineValue)).toMatchJsValue(mathJsValue);
  });
});

matrix3x3.forEach(v => {
  it(`abs ${v.title}`, () => {
    const mathJsValue = mathjs.det(v.jsValue);
    expect(SciLine.abs(v.sciLineValue)).toMatchJsValue(mathJsValue);
  });
});
