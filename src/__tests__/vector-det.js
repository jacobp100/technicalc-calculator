const { toMatchJsValue } = require("../__test-util__");
const Types = require("../Types.bs");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

it(`det [3 ,7, 8]`, () => {
  expect(
    SciLine.abs(Types.vector3(...[3, 7, 8].map(SciLine.ofFloat)))
  ).toMatchJsValue(122);
});
