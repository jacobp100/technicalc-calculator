const rat = require("big-rat");
const cartesian = require("cartesian");
const { magnitude } = require("../Util/QUtil.bs");

describe("calculates q magnitude", () => {
  const values = [
    "1",
    "2",
    "5",
    "10",
    "53",
    "100",
    "208",
    "1000",
    "3918",
    "48303",
    "5842753",
    "95482053253",
    "9584205482539355423",
    "8549051290485403254302594230548325432043225",
    "100000000000000000000000000000000000000000000000000000000000000000"
  ];

  cartesian([[1, -1], values, values]).forEach(([mul, num, den]) => {
    const sign = mul === 1 ? "" : "-";

    it(`for ${sign}${num}/${den}`, () => {
      const q = rat(sign + num, den);
      const mag = magnitude(q);
      const jsMag = +(mul * (+num / +den)).toExponential().split("e")[1];
      expect(mag).toBe(jsMag);
    });
  });
});
