const { ofVector, ofFloat, abs } = require("./src/Value.bs");
const { toString } = require("./src/ValueTest.bs");

console.log(toString(abs(ofVector([3, 7, 8].map(ofFloat)))));
