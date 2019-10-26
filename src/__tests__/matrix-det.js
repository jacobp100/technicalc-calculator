const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(matrix2x2)("abs(%s)", v => {
  const actual = SciLine.abs(v.sciLineValue);
  const expected = mathjs.det(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});

test.each(matrix3x3)("abs(%s)", v => {
  const actual = SciLine.abs(v.sciLineValue);
  const expected = mathjs.det(v.jsValue);
  expect(actual).toMatchJsValue(expected);
});
