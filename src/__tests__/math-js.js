const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value, toMatchJsValue } = require("../testUtil");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

const range12 = Array.from({ length: 12 }, (_, i) => i + 1);

const trigValues = cartesian([range12, range12, range12, range12]).map(
  ([numRe, denomRe, numIm, denomIm]) =>
    new Value(
      mathjs.complex((Math.PI * numRe) / denomRe, (Math.PI * numIm) / denomIm),
      SciLine.mul(
        SciLine.add(
          SciLine.div(SciLine.of_float(numRe), SciLine.of_float(denomRe)),
          SciLine.mul(
            SciLine.div(SciLine.of_float(numIm), SciLine.of_float(denomIm)),
            SciLine.i
          )
        ),
        SciLine.pi
      ),
      `(${numRe}/${denomRe}+${numIm}i/${denomIm})pi`
    )
);

it("works for sin", () => {
  trigValues.forEach(v => {
    expect(SciLine.sin(v.sciLineValue)).toMatchJsValue(
      mathjs.sin(v.jsValue),
      v.title
    );
  });
});
