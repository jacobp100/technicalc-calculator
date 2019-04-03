const {
  to_string: toString,
  to_latex: toLatex,
  to_mml: toMml,
  resolve,
  one,
  minus_one: minusOne,
  of_string: ofString,
  add,
  mul,
  pi,
  i
} = require("../SciLine.bs");

const [three, minusThree, threeHalves, minusThreeHalves, half, minusHalf] = [
  "3",
  "-3",
  "1.5",
  "-1.5",
  "0.5",
  "-0.5"
].map(ofString);

it("Formats strings", () => {
  expect(toString(resolve(one))).toBe("1");
  expect(toString(resolve(minusOne))).toBe("-1");
  expect(toString(resolve(half))).toBe("1/2");
  expect(toString(resolve(minusHalf))).toBe("-1/2");
  expect(toString(resolve(mul(one, i)))).toBe("i");
  expect(toString(resolve(mul(minusOne, i)))).toBe("-i");
  expect(toString(resolve(mul(half, i)))).toBe("1/2i");
  expect(toString(resolve(mul(minusHalf, i)))).toBe("-1/2i");
  expect(toString(resolve(add(one, mul(one, i))))).toBe("1+i");
  expect(toString(resolve(add(minusOne, mul(one, i))))).toBe("-1+i");
  expect(toString(resolve(add(one, mul(minusOne, i))))).toBe("1-i");
  expect(toString(resolve(add(minusOne, mul(minusOne, i))))).toBe("-1-i");
  expect(toString(resolve(mul(one, pi)))).toBe("pi");
  expect(toString(resolve(mul(minusOne, pi)))).toBe("-pi");
  expect(toString(resolve(mul(half, pi)))).toBe("pi/2");
  expect(toString(resolve(mul(minusHalf, pi)))).toBe("-pi/2");
  expect(toString(resolve(mul(three, pi)))).toBe("3pi");
  expect(toString(resolve(mul(minusThree, pi)))).toBe("-3pi");
  expect(toString(resolve(mul(threeHalves, pi)))).toBe("3pi/2");
  expect(toString(resolve(mul(minusThreeHalves, pi)))).toBe("-3pi/2");
});

it("Formats latex", () => {
  expect(toLatex(resolve(one))).toBe("1");
  expect(toLatex(resolve(minusOne))).toBe("-1");
  expect(toLatex(resolve(half))).toBe("\\frac{1}{2}");
  expect(toLatex(resolve(minusHalf))).toBe("-\\frac{1}{2}");
  expect(toLatex(resolve(add(one, mul(one, i))))).toBe("1+i");
  expect(toLatex(resolve(add(minusOne, mul(one, i))))).toBe("-1+i");
  expect(toLatex(resolve(add(one, mul(minusOne, i))))).toBe("1-i");
  expect(toLatex(resolve(add(minusOne, mul(minusOne, i))))).toBe("-1-i");
  expect(toLatex(resolve(mul(one, pi)))).toBe("\\pi");
  expect(toLatex(resolve(mul(minusOne, pi)))).toBe("-\\pi");
  expect(toLatex(resolve(mul(half, pi)))).toBe("\\frac{\\pi}{2}");
  expect(toLatex(resolve(mul(minusHalf, pi)))).toBe("-\\frac{\\pi}{2}");
  expect(toLatex(resolve(mul(three, pi)))).toBe("3\\pi");
  expect(toLatex(resolve(mul(minusThree, pi)))).toBe("-3\\pi");
  expect(toLatex(resolve(mul(threeHalves, pi)))).toBe("\\frac{3\\pi}{2}");
  expect(toLatex(resolve(mul(minusThreeHalves, pi)))).toBe("-\\frac{3\\pi}{2}");
});

it("Formats mathml", () => {
  expect(toMml(resolve(one))).toBe("<mn>1</mn>");
  expect(toMml(resolve(minusOne))).toBe("<mo>-</mo><mn>1</mn>");
  expect(toMml(resolve(half))).toBe("<mfrac><mn>1</mn><mn>2</mn></mfrac>");
  expect(toMml(resolve(minusHalf))).toBe(
    "<mo>-</mo><mfrac><mn>1</mn><mn>2</mn></mfrac>"
  );
  expect(toMml(resolve(add(one, mul(one, i))))).toBe(
    "<mn>1</mn><mo>+</mo><mi>i</mi>"
  );
  expect(toMml(resolve(add(minusOne, mul(one, i))))).toBe(
    "<mo>-</mo><mn>1</mn><mo>+</mo><mi>i</mi>"
  );
  expect(toMml(resolve(add(one, mul(minusOne, i))))).toBe(
    "<mn>1</mn><mo>-</mo><mi>i</mi>"
  );
  expect(toMml(resolve(add(minusOne, mul(minusOne, i))))).toBe(
    "<mo>-</mo><mn>1</mn><mo>-</mo><mi>i</mi>"
  );
  expect(toMml(resolve(mul(one, pi)))).toBe("<mi>&pi;</mi>");
  expect(toMml(resolve(mul(minusOne, pi)))).toBe("<mo>-</mo><mi>&pi;</mi>");
  expect(toMml(resolve(mul(half, pi)))).toBe(
    "<mfrac><mi>&pi;</mi><mn>2</mn></mfrac>"
  );
  expect(toMml(resolve(mul(minusHalf, pi)))).toBe(
    "<mo>-</mo><mfrac><mi>&pi;</mi><mn>2</mn></mfrac>"
  );
  expect(toMml(resolve(mul(three, pi)))).toBe("<mn>3</mn><mi>&pi;</mi>");
  expect(toMml(resolve(mul(minusThree, pi)))).toBe(
    "<mo>-</mo><mn>3</mn><mi>&pi;</mi>"
  );
  expect(toMml(resolve(mul(threeHalves, pi)))).toBe(
    "<mfrac><mrow><mn>3</mn><mi>&pi;</mi></mrow><mn>2</mn></mfrac>"
  );
  expect(toMml(resolve(mul(minusThreeHalves, pi)))).toBe(
    "<mo>-</mo><mfrac><mrow><mn>3</mn><mi>&pi;</mi></mrow><mn>2</mn></mfrac>"
  );
});
