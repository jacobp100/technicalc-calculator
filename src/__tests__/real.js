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
  expect(convert("-1")).toBe("-1");
  expect(convert("-1e2")).toBe("-100");
  expect(convert("-1e+2")).toBe("-100");
  expect(convert("-1e-2")).toBe("-1/100");
  expect(convert("-1e1000")).toBe("-1e1000");
  expect(convert("-1.23456789")).toBe("-1.23456789");
  expect(convert("-1.23456789e-100")).toBe("-1.23456789e-100");
  expect(convert("-1.23456789e100")).toBe("-1.23456789e100");
});

it("Converts decimals to fractions", () => {
  expect(resultString(string("0.4"))).toBe("2/5");
  expect(resultString(string("0.123456789"))).toBe("0.123456789");
});

it("Cannot handle imaginary numbers", () => {
  expect(resultString(Real.sqrt(float(-1)))).toBe("NaN");
  expect(resultString(Real.log(float(-1)))).toBe("NaN");
  expect(resultString(Real.pow(float(-1), float(0.5)))).toBe("NaN");
  expect(resultString(Real.asin(float(-2)))).toBe("NaN");
  expect(resultString(Real.asin(float(2)))).toBe("NaN");
  expect(resultString(Real.acos(float(-2)))).toBe("NaN");
  expect(resultString(Real.acos(float(2)))).toBe("NaN");
  expect(resultString(Real.acosh(float(0)))).toBe("NaN");
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
    "0.841470984807"
  );
});

it("Formats various numbers correctly", () => {
  const convert = x => resultString(Real.of_string(undefined, x));
  expect(convert("46.47897327055571")).toBe("46.478973270555");
  expect(convert("-47.86759243619015")).toBe("-47.86759243619");
  expect(convert("7.712346515387281")).toBe("7.712346515387");
  expect(convert("-41.08525582534328")).toBe("-41.085255825343");
  expect(convert("24.036159870635387")).toBe("24.036159870635");
  expect(convert("21.622267655248706")).toBe("21.622267655248");
  expect(convert("85.87032800784263")).toBe("85.870328007842");
  expect(convert("6.759552690635729")).toBe("6.759552690635");
  expect(convert("17.724509834485048")).toBe("17.724509834485");
  expect(convert("-17.618661853244163")).toBe("-17.618661853244");
  expect(convert("-71.09059285654436")).toBe("-71.090592856544");
  expect(convert("47.438865505981084")).toBe("47.438865505981");
  expect(convert("-18.28378337248739")).toBe("-18.283783372487");
  expect(convert("71.18764618368766")).toBe("71.187646183687");
  expect(convert("-66.96121712260108")).toBe("-66.961217122601");
  expect(convert("28.50749266445851")).toBe("28.507492664458");
  expect(convert("16.454703415645668")).toBe("16.454703415645");
  expect(convert("-64.43380990868866")).toBe("-64.433809908688");
  expect(convert("-66.90487607393479")).toBe("-66.904876073934");
  expect(convert("-28.212396089967342")).toBe("-28.212396089967");
});
