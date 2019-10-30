const { one, minusOne, ofString, add, mul, pi, i } = require("../Value.bs");
const { toString } = require("../ScilineTest.bs");

const [three, minusThree, threeHalves, minusThreeHalves, half, minusHalf] = [
  "3",
  "-3",
  "1.5",
  "-1.5",
  "0.5",
  "-0.5"
].map(ofString);

const format = (value, mode) =>
  toString(value, mode != null ? { mode } : undefined);

it("Formats strings", () => {
  expect(format(one)).toBe("1");
  expect(format(minusOne)).toBe("-1");
  expect(format(half)).toBe("1/2");
  expect(format(minusHalf)).toBe("-1/2");
  expect(format(i)).toBe("i");
  expect(format(mul(minusOne, i))).toBe("-i");
  expect(format(mul(half, i))).toBe("1/2i");
  expect(format(mul(minusHalf, i))).toBe("-1/2i");
  expect(format(add(one, i))).toBe("1+i");
  expect(format(add(minusOne, i))).toBe("-1+i");
  expect(format(add(one, mul(minusOne, i)))).toBe("1-i");
  expect(format(add(minusOne, mul(minusOne, i)))).toBe("-1-i");
  expect(format(mul(one, pi))).toBe("pi");
  expect(format(mul(minusOne, pi))).toBe("-pi");
  expect(format(mul(half, pi))).toBe("pi/2");
  expect(format(mul(minusHalf, pi))).toBe("-pi/2");
  expect(format(mul(three, pi))).toBe("3pi");
  expect(format(mul(minusThree, pi))).toBe("-3pi");
  expect(format(mul(threeHalves, pi))).toBe("3pi/2");
  expect(format(mul(minusThreeHalves, pi))).toBe("-3pi/2");
});

it("Formats Tex", () => {
  expect(format(one, "tex")).toBe("1");
  expect(format(minusOne, "tex")).toBe("-1");
  expect(format(half, "tex")).toBe("\\frac{1}{2}");
  expect(format(minusHalf, "tex")).toBe("-\\frac{1}{2}");
  expect(format(add(one, i), "tex")).toBe("1+i");
  expect(format(add(minusOne, i), "tex")).toBe("-1+i");
  expect(format(add(one, mul(minusOne, i)), "tex")).toBe("1-i");
  expect(format(add(minusOne, mul(minusOne, i)), "tex")).toBe("-1-i");
  expect(format(mul(one, pi), "tex")).toBe("\\pi");
  expect(format(mul(minusOne, pi), "tex")).toBe("-\\pi");
  expect(format(mul(half, pi), "tex")).toBe("\\frac{\\pi}{2}");
  expect(format(mul(minusHalf, pi), "tex")).toBe("-\\frac{\\pi}{2}");
  expect(format(mul(three, pi), "tex")).toBe("3\\pi");
  expect(format(mul(minusThree, pi), "tex")).toBe("-3\\pi");
  expect(format(mul(threeHalves, pi), "tex")).toBe("\\frac{3\\pi}{2}");
  expect(format(mul(minusThreeHalves, pi), "tex")).toBe("-\\frac{3\\pi}{2}");
});

it("Formats mathml", () => {
  const math = x =>
    `<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">${x}</math>`;
  expect(format(one, "mathml")).toBe(math("<mn>1</mn>"));
  expect(format(minusOne, "mathml")).toBe(math("<mo>-</mo><mn>1</mn>"));
  expect(format(half, "mathml")).toBe(
    math("<mfrac><mn>1</mn><mn>2</mn></mfrac>")
  );
  expect(format(minusHalf, "mathml")).toBe(
    math("<mo>-</mo><mfrac><mn>1</mn><mn>2</mn></mfrac>")
  );
  expect(format(add(one, i), "mathml")).toBe(
    math("<mn>1</mn><mo>+</mo><mi>i</mi>")
  );
  expect(format(add(minusOne, i), "mathml")).toBe(
    math("<mo>-</mo><mn>1</mn><mo>+</mo><mi>i</mi>")
  );
  expect(format(add(one, mul(minusOne, i)), "mathml")).toBe(
    math("<mn>1</mn><mo>-</mo><mi>i</mi>")
  );
  expect(format(add(minusOne, mul(minusOne, i)), "mathml")).toBe(
    math("<mo>-</mo><mn>1</mn><mo>-</mo><mi>i</mi>")
  );
  expect(format(mul(one, pi), "mathml")).toBe(math("<mi>&pi;</mi>"));
  expect(format(mul(minusOne, pi), "mathml")).toBe(
    math("<mo>-</mo><mi>&pi;</mi>")
  );
  expect(format(mul(half, pi), "mathml")).toBe(
    math("<mfrac><mi>&pi;</mi><mn>2</mn></mfrac>")
  );
  expect(format(mul(minusHalf, pi), "mathml")).toBe(
    math("<mo>-</mo><mfrac><mi>&pi;</mi><mn>2</mn></mfrac>")
  );
  expect(format(mul(three, pi), "mathml")).toBe(
    math("<mn>3</mn><mi>&pi;</mi>")
  );
  expect(format(mul(minusThree, pi), "mathml")).toBe(
    math("<mo>-</mo><mn>3</mn><mi>&pi;</mi>")
  );
  expect(format(mul(threeHalves, pi), "mathml")).toBe(
    math("<mfrac><mrow><mn>3</mn><mi>&pi;</mi></mrow><mn>2</mn></mfrac>")
  );
  expect(format(mul(minusThreeHalves, pi), "mathml")).toBe(
    math(
      "<mo>-</mo><mfrac><mrow><mn>3</mn><mi>&pi;</mi></mrow><mn>2</mn></mfrac>"
    )
  );

  expect(format(one, "mathml-inline")).toBe(
    '<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline"><mn>1</mn></math>'
  );
});
