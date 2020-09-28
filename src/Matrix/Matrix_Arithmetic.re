open Matrix_Types;
open Matrix_Base;

let add = (a: t, b: t): t =>
  switch (zipBy(a, b, Scalar.add)) {
  | Some(m) => m
  | None => empty
  };

let sub = (a: t, b: t): t =>
  switch (zipBy(a, b, Scalar.sub)) {
  | Some(m) => m
  | None => empty
  };

let mul = (a: t, b: t): t =>
  if (a.numColumns == b.numColumns) {
    let shape = a.numColumns;
    makeByU(
      shape,
      shape,
      (. row, column) => {
        let element = ref(`Zero);
        for (i in 0 to shape - 1) {
          let elementProduct =
            Scalar.mul(getExn(a, row, i), getExn(b, i, column));
          element := Scalar.add(element^, elementProduct);
        };
        element^;
      },
    );
  } else {
    empty;
  };
