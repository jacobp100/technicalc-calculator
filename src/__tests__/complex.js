const Complex = require("../Scalar/Complex.bs");

const stringOf = Complex.to_string;
const floats = Complex.of_floats;

it("Special cases square root of negative numbers", () => {
  expect(stringOf(Complex.sqrt(floats(-2, 0)))).toBe("sqrt(2) i");
});
