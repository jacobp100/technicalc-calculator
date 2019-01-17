# SciLine Calculator

A math library similar to math-js. It tracks constants (Pi, sqrt() and exp()) values for more accurate results.

### Compiling

To compile, `yarn build`, or `yarn start` to enter watch mode.

### Testing

To test, build in watch mode (`yarn start`), and in another terminal tab, `yarn test`. Testing is mostly fuzz testing against math-js and the native math library, and the tests can take minutes to complete.

Collecting coverage is very useful to check which branches are called - especially the HTML outputs. However, as the JS files are compiled, an actual metric is not too useful. To get coverage, run `yarn test --coverage`, and check the `/coverage` folder.
