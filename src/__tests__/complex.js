const Complex = require("../Scalar/Complex.bs");

const stringOf = Complex.toString.bind(null, undefined);
const floats = Complex.ofFloats;

it("Special cases square root of negative numbers", () => {
  expect(stringOf(Complex.sqrt(floats(-2, 0)))).toBe("sqrt(2)i");
});
