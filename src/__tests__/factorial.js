const { range } = require("lodash");
const mathjs = require("mathjs");
const { Value, toMatchJsValue } = require("../__test-util__");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

range(0, 20 + 1)
  .map(Value.float)
  .forEach(v => {
    it(`fact ${v.title}`, () => {
      expect(SciLine.factorial(v.sciLineValue)).toMatchJsValue(
        mathjs.factorial(v.jsValue)
      );
    });
  });
