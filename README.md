# TechniCalc Calculator

A math library similar to math-js, built in ReasonML. It supports complex numbers and matrices. Designed for use in the [TechniCalc iOS app](https://apps.apple.com/gb/app/technicalc-calculator/id1504965415).

It uses rational numbers most of the time. When it is using rational numbers, it tracks some (Pi, sqrt() and exp()) values for more accurate results. For example, `sin(pi)` is exactly `1`, but this is not the output when using floating point arithmetic. In addition, special trig values can be output, like `sin(pi / 2)` giving `sqrt(2)/2`.

When a number cannot be expressed as a rational, we represent it as a decimal using the decimal.js library. When this happens, precision is lost, so it is hard to go back to the rational representation (although multiplying by zero will do it).

The final output can either be a fraction with up to one tracked value, or float.

The aim is to not be a symbolic calculator, but to take some concepts from symbolic calculation to improve the accuracy.

### Compiling

To compile, `yarn build`, or `yarn start` to enter watch mode.

### Testing

To test, build in watch mode (`yarn start`), and in another terminal tab, `yarn test`. Testing is mostly fuzz testing against math-js and the native math library, and the tests can take minutes to complete.

Collecting coverage is very useful to check which branches are called - especially the HTML outputs. However, as the JS files are compiled, an actual metric is not too useful. To get coverage, run `yarn test --coverage`, and check the `/coverage` folder.
