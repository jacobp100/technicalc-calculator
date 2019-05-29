open Types;
open Base;

let steffanRoot = (f, x) => {
  let maxI = 20;
  let i = ref(0);
  let x = ref(x);
  while (i^ < maxI) {
    let fx = f(x^);
    let gx = fx != zero ? f(x^ + fx) / fx - one : zero;
    if (gx == zero || gx == nan) {
      i := maxI;
    } else {
      i := Pervasives.(i^ + 1);
      x := x^ - fx / gx;
    };
  };
  x^;
};
