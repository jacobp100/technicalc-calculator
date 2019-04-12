const BN = require("bn.js");
const rat = require("big-rat");
const NumberFormat = require("../NumberFormat.bs");

it("should format exponentials without a custom exponent format", () => {
  const format = NumberFormat.createFormat();
  expect(
    NumberFormat.formatExponential(undefined, new BN(1), format, rat(10))
  ).toEqual(["1", "1"]);
});
