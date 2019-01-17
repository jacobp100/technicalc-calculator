# SciLine Calculator

A math library similar to math-js, built in ReasonML. It supports complex numbers and matrices. Designed for use in the [SciLine iOS app](https://itunes.apple.com/sg/app/sciline-scientific-engineering-calculator/id1219218410?mt=8).

It uses Zarith rational numbers (or big-rat and bn.js when using Bucklescript).

It tracks some (Pi, sqrt() and exp()) values for more accurate results. For example, `sin(pi)` is exactly `1`, but this is not the output when using floating point arithmetic. In addition, special trig values can be output, like `sin(pi / 2)` giving `sqrt(2)/2`.

The final output can either be a fraction with up to one tracked value, or float.

The aim is to not be a symbolic calculator, but to take some concepts from symbolic calculation to improve the accuracy.

### Compiling

To compile, `yarn build`, or `yarn start` to enter watch mode.

### Testing

To test, build in watch mode (`yarn start`), and in another terminal tab, `yarn test`. Testing is mostly fuzz testing against math-js and the native math library, and the tests can take minutes to complete.

Collecting coverage is very useful to check which branches are called - especially the HTML outputs. However, as the JS files are compiled, an actual metric is not too useful. To get coverage, run `yarn test --coverage`, and check the `/coverage` folder.
