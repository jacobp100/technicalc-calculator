const Real = require("../Scalar/Real.bs");

const float = Real.of_float.bind(null, undefined);
const stringOf = Real.to_string;

it("Converts decimals to fractions", () => {
  expect(stringOf(float(0.4))).toBe("2/5");
  expect(stringOf(float(0.123456789))).toBe("0.123456789");
});

it("Cannot handle imaginary numbers", () => {
  expect(stringOf(Real.sqrt(float(-1)))).toBe("NaN");
  expect(stringOf(Real.log(float(-1)))).toBe("NaN");
  expect(stringOf(Real.pow(float(-1), float(0.5)))).toBe("NaN");
  expect(stringOf(Real.arcsin(float(-2)))).toBe("NaN");
  expect(stringOf(Real.arcsin(float(2)))).toBe("NaN");
  expect(stringOf(Real.arccos(float(-2)))).toBe("NaN");
  expect(stringOf(Real.arccos(float(2)))).toBe("NaN");
  expect(stringOf(Real.arccosh(float(0)))).toBe("NaN");
});

it("Simplifies division by two square roots", () => {
  expect(stringOf(Real.div(Real.sqrt(float(10)), Real.sqrt(float(2))))).toEqual(
    "sqrt(5)"
  );
  expect(
    stringOf(Real.div(Real.sqrt(float(1000)), Real.sqrt(float(2))))
  ).toEqual("10sqrt(5)");
});

it("Tracks exp values through log", () => {
  expect(stringOf(Real.log(Real.exp(float(47))))).toEqual("47");
});

it("Simplifies square roots and exponentials", () => {
  expect(stringOf(Real.mul(float(2), Real.sqrt(float(2))))).toEqual("2sqrt(2)");
  expect(stringOf(Real.sqrt(float(1000)))).toEqual("10sqrt(10)");
  expect(stringOf(Real.sqrt(float(4)))).toEqual("2");
  expect(stringOf(Real.sqrt(float(8)))).toEqual("2sqrt(2)");
  expect(stringOf(Real.sqrt(float(6)))).toEqual("sqrt(6)");
  expect(stringOf(Real.sqrt(float(12)))).toEqual("2sqrt(3)");
  expect(stringOf(Real.sqrt(float(0)))).toEqual("0");
  expect(stringOf(Real.exp(float(0)))).toEqual("1");
  expect(stringOf(Real.exp(float(1)))).toEqual("exp(1)");
  expect(stringOf(Real.exp(float(2)))).toEqual("exp(2)");
  expect(stringOf(Real.exp(float(3)))).toEqual("exp(3)");
  expect(stringOf(Real.exp(float(-1)))).toEqual("exp(-1)");
});

it("Does not simplify pi", () => {
  expect(stringOf(Real.pi)).toEqual("pi");
});
