module Make = (Number: Types.Scalar) => {
  let (+) = Number.add;
  let (-) = Number.sub;
  let ( * ) = Number.mul;
  let (/) = Number.div;
  let (~-) = Number.neg;

  type t = {
    numRows: int,
    numColumns: int,
    elements: array(Number.t),
  };

  let nan = {numRows: 0, numColumns: 0, elements: [||]};
  let is_nan = a => Array.length(a.elements) == 0;

  let normalize = a =>
    Array.fold_left(
      (current, e) => Number.is_nan(e) ? nan : current,
      a,
      a.elements,
    );

  let _map_elements = (iter, a) =>
    normalize({...a, elements: Array.map(iter, a.elements)});

  let _combine = (iter, a, b) =>
    if (a.numRows == b.numRows && a.numColumns == b.numColumns) {
      let elements =
        Array.mapi(
          (i, _) => iter(a.elements[i], b.elements[i]),
          a.elements,
        );
      normalize({...a, elements});
    } else {
      nan;
    };

  let from_elements = (~rows as numRows, ~columns as numColumns, elements) =>
    normalize({numRows, numColumns, elements});

  let mul_const = (c, a) => _map_elements(Number.mul(c), a);
  let div_const = (c, a) => _map_elements(Number.div(c), a);

  let add = _combine(Number.add);
  let sub = _combine(Number.sub);

  let mul = (a, b) => nan;
  let div = (a, b) => nan;

  let neg = _map_elements(Number.neg);
};
