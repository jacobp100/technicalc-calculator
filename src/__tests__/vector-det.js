const { toMatchJsValue } = require("../__test-util__");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

it(`det [3 ,7, 8]`, () => {
  expect(
    SciLine.abs(SciLine.vector3(...[3, 7, 8].map(SciLine.ofFloat)))
  ).toMatchJsValue(122);
});
