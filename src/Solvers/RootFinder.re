open Types;
open Base;

let steffensenRoot = (f, x) => {
  let realIterations = 3;
  let floatIterations = 50;

  let rec resolveReal = (~iterations=realIterations, x) => {
    let _ = [%raw "console.warn('real ' + iterations)"];
    let fx = f(x);
    if (fx == zero) {
      x;
    } else if (iterations > 0) {
      let gx = f(x + fx) / fx - one;
      if (gx != nan) {
        let iterations = Pervasives.(iterations - 1);
        let x' = x - fx / gx;
        resolveReal(~iterations, x');
      } else {
        nan;
      };
    } else {
      resolveFloat(~iterations=floatIterations, toFloat(x));
    };
  }
  and resolveFloat = (~iterations, x) => {
    let _ = [%raw "console.warn('float ' + _iterations + ' (' + x$2 + ')')"];
    let x' = real(Q.of_float(x));
    let fx' = f(x');
    if (fx' == zero) {
      x';
    } else if (iterations > 0) {
      let iterations = Pervasives.(iterations - 1);
      let fx = toFloat(fx');
      let gx = f(real(Q.of_float(x +. fx)))->toFloat /. fx -. 1.;
      let x = x -. fx /. gx;
      if (FloatUtil.isFinite(x)) {
        resolveFloat(~iterations, x);
      } else {
        nan;
      };
    } else {
      let delta = toFloat(fx')->abs_float /. x;
      if (delta < 0.01) {
        x';
      } else {
        nan;
      };
    };
  };

  resolveReal(x);
};
