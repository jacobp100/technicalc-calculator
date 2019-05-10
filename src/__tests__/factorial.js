const cartesian = require("cartesian");
const { range } = require("lodash");
const mathjs = require("mathjs");
const { Value, toMatchJsValue } = require("../__test-util__");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

const values = [...range(-2, 2 + 0.1, 0.1), ...range(2, 6 + 0.5, 0.5)];
cartesian([values, values])
  .map(([re, im]) => Value.complex(re, im))
  .forEach(v => {
    it(`fact ${v.title}`, () => {
      expect(SciLine.factorial(v.sciLineValue)).toMatchJsValue(
        mathjs.gamma(mathjs.complex(v.jsValue.re + 1, v.jsValue.im))
      );
    });
  });
