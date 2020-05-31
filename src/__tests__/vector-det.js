const { toMatchJsValue } = require("../__test-util__");
const Types = require("../Types.bs");
const TechniCalc = require("../Value.bs");

expect.extend({ toMatchJsValue });

it(`det [3 ,7, 8]`, () => {
  expect(
    TechniCalc.abs(Types.vector3(...[3, 7, 8].map(TechniCalc.ofFloat)))
  ).toMatchJsValue(122);
});
