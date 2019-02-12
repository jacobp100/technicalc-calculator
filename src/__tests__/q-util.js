const rat = require("big-rat");
const cartesian = require("cartesian");
const { magnitude } = require("../Math/QUtil.bs");

it("calculates q magnitude", () => {
  const values = [
    1,
    2,
    5,
    53,
    208,
    3918,
    48303,
    5842753,
    95482053253,
    9584205482539355423,
    8549051290485403254302594230548325432043225
  ];

  cartesian([[1, -1], values, values]).forEach(([mul, num, den]) => {
    const q = rat(mul * num, den);
    const mag = magnitude(q);
    const jsMag = Number((mul * (num / den)).toExponential().split("e")[1]);
    expect(mag).toBe(jsMag);
  });
});
