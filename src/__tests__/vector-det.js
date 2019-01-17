const { toMatchJsValue } = require("../__test-util__");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

it(`det [3 ,7, 8]`, () => {
  expect(
    SciLine.abs(
      SciLine.matrix_of_elements(3, 1, [3, 7, 8].map(SciLine.of_float))
    )
  ).toMatchJsValue(122);
});
