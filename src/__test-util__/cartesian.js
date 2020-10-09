const baseCartesian = require("cartesian");
const { sampleSize } = require("lodash");


module.exports = (a, b) => sampleSize(baseCartesian(a, b), 20)
