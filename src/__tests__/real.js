const Real = require("../Scalar/Real.bs");

const string = Real.of_string.bind(null, undefined);
const float = Real.of_float.bind(null, undefined);
const resultString = Real.to_string.bind(null, undefined);

it("Converts via of_string", () => {
  const convert = x => resultString(Real.of_string(undefined, x));
  expect(convert("1")).toBe("1");
  expect(convert("1e2")).toBe("100");
  expect(convert("1e+2")).toBe("100");
  expect(convert("1e-2")).toBe("1/100");
  expect(convert("1e1000")).toBe("1e1000");
  expect(convert("1.23456789")).toBe("1.23456789");
  expect(convert("1.23456789e-100")).toBe("1.23456789e-100");
  expect(convert("1.23456789e100")).toBe("1.23456789e100");
});

it("Converts decimals to fractions", () => {
  expect(resultString(string("0.4"))).toBe("2/5");
  expect(resultString(string("0.123456789"))).toBe("0.123456789");
});

it("Cannot handle imaginary numbers", () => {
  expect(resultString(Real.sqrt(float(-1)))).toBe("NaN");
  expect(resultString(Real.log(float(-1)))).toBe("NaN");
  expect(resultString(Real.pow(float(-1), float(0.5)))).toBe("NaN");
  expect(resultString(Real.arcsin(float(-2)))).toBe("NaN");
  expect(resultString(Real.arcsin(float(2)))).toBe("NaN");
  expect(resultString(Real.arccos(float(-2)))).toBe("NaN");
  expect(resultString(Real.arccos(float(2)))).toBe("NaN");
  expect(resultString(Real.arccosh(float(0)))).toBe("NaN");
});

it("Simplifies division by two square roots", () => {
  expect(
    resultString(Real.div(Real.sqrt(float(10)), Real.sqrt(float(2))))
  ).toEqual("sqrt(5)");
  expect(
    resultString(Real.div(Real.sqrt(float(1000)), Real.sqrt(float(2))))
  ).toEqual("10sqrt(5)");
});

it("Tracks exp values through log", () => {
  expect(resultString(Real.log(Real.exp(float(47))))).toEqual("47");
});

it("Simplifies square roots and exponentials", () => {
  expect(resultString(Real.mul(float(2), Real.sqrt(float(2))))).toEqual(
    "2sqrt(2)"
  );
  expect(resultString(Real.sqrt(float(1000)))).toEqual("10sqrt(10)");
  expect(resultString(Real.sqrt(float(4)))).toEqual("2");
  expect(resultString(Real.sqrt(float(8)))).toEqual("2sqrt(2)");
  expect(resultString(Real.sqrt(float(6)))).toEqual("sqrt(6)");
  expect(resultString(Real.sqrt(float(12)))).toEqual("2sqrt(3)");
  expect(resultString(Real.sqrt(float(0)))).toEqual("0");
  expect(resultString(Real.exp(float(0)))).toEqual("1");
  expect(resultString(Real.exp(float(1)))).toEqual("exp(1)");
  expect(resultString(Real.exp(float(2)))).toEqual("exp(2)");
  expect(resultString(Real.exp(float(3)))).toEqual("exp(3)");
  expect(resultString(Real.exp(float(-1)))).toEqual("exp(-1)");
});

it("Does not simplify pi", () => {
  expect(resultString(Real.pi)).toEqual("pi");
});

it("Takes sin of pi + 1", () => {
  expect(Real.to_string(undefined, Real.sin(Real.add(Real.pi, float(1))))).toBe(
    "-9.585290151921e-1"
  );
});
