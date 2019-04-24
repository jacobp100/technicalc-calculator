const { ofInt } = require("../Value/Types.bs");
const { sqrt, toString } = require("../Value/Value.bs");

it("Special cases square root of negative numbers", () => {
  expect(toString(undefined, sqrt(ofInt(-2)))).toBe("sqrt(2)i");
});
