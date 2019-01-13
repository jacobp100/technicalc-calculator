module Make = (Number: Types.Scalar) => {
  let (==) = Number.equal;
  let (+) = Number.add;
  let (-) = Number.sub;
  let ( * ) = Number.mul;
  let (/) = Number.div;
  let (~-) = Number.neg;
  let (==%) = Pervasives.(==);
  let (+%) = Pervasives.(+);
  let (-%) = Pervasives.(-);
  let ( *% ) = Pervasives.( * );
  let (/%) = Pervasives.(/);
  let (~-%) = Pervasives.(~-);

  type t = {
    numRows: int,
    numColumns: int,
    elements: array(Number.t),
  };

  let init = (~numRows, ~numColumns, elements) => {
    numRows,
    numColumns,
    elements,
  };

  let nan = {numRows: 0, numColumns: 0, elements: [||]};
  let is_nan = a => Array.length(a.elements) ==% 0;

  let to_array_matrix = a => a.elements;

  let shape_equal = (a, b) =>
    a.numRows ==% b.numRows && a.numColumns ==% b.numColumns;

  let equal = (a, b) =>
    if (shape_equal(a, b)) {
      let elements_match = ref(true);
      Array.iteri(
        (i, _) =>
          if (!(a.elements[i] == b.elements[i])) {
            elements_match := false;
          },
        a.elements,
      );
      elements_match^;
    } else {
      false;
    };

  let normalize = a =>
    Array.fold_left(
      (current, e) => Number.is_nan(e) ? nan : current,
      a,
      a.elements,
    );

  let _map_elements = (iter, a) =>
    normalize({...a, elements: Array.map(iter, a.elements)});

  let _combine = (iter, a, b) =>
    if (shape_equal(a, b)) {
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

  let _element_index = (~numRows, row, column) => column +% row *% numRows;

  let mul = (a, b) =>
    switch (a, b) {
    | (
        {numRows: 3, numColumns: 1, elements: [|a1, a2, a3|]},
        {numRows: 3, numColumns: 1, elements: [|b1, b2, b3|]},
      ) => {
        numRows: 3,
        numColumns: 1,
        elements: [|a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1|],
      }
    | (_, _) when a.numColumns ==% b.numRows =>
      let shape = a.numRows;
      let elements =
        Array.init(
          shape *% shape,
          elementIndex => {
            let column = elementIndex mod shape;
            let row = elementIndex /% shape;
            let element = ref(Number.zero);
            for (i in 0 to shape -% 1) {
              let aIndex = _element_index(~numRows=shape, row, i);
              let bIndex = _element_index(~numRows=shape, i, column);
              element := element^ + a.elements[aIndex] * b.elements[bIndex];
            };
            element^;
          },
        );
      {numRows: shape, numColumns: shape, elements};
    | _ => nan
    };

  let dot = (a, b) =>
    switch (a, b) {
    | ({numColumns: 1, elements: aElem}, {numColumns: 1, elements: bElem}) =>
      let accum = ref(Number.zero);
      Array.iteri((i, _) => accum := accum^ + aElem[i] * bElem[i], aElem);
      accum^;
    | _ => Number.nan
    };

  let div = (_a, _b) => nan;

  let det = a =>
    switch (a) {
    | {numColumns: 1} =>
      a.elements
      |> Array.map(Number.pow(_, Number.of_int(2)))
      |> Array.fold_left(Number.add, Number.zero)
    | {numRows: 2, numColumns: 2, elements: [|a, b, c, d|]} => a * d - b * c
    | {numRows: 3, numColumns: 3, elements: [|a, b, c, d, e, f, g, h, i|]} =>
      /* https://www.wolframalpha.com/input/?i=det(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D) */
      a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g
    | _ => Number.nan
    };

  let neg = _map_elements(Number.neg);

  let _joined_elements = (elements, join, fn) =>
    String.concat(join, Array.to_list(Array.init(elements, fn)));

  let to_string = a => {
    let bracketed_elements = (elements, fn) =>
      "{" ++ _joined_elements(elements, ", ", fn) ++ "}";
    let create_row = row =>
      bracketed_elements(a.numColumns, column =>
        Number.to_string(a.elements[a.numColumns *% row +% column])
      );
    bracketed_elements(a.numRows, create_row);
  };

  let to_latex = a => {
    let create_row = row =>
      _joined_elements(a.numColumns, " && ", column =>
        Number.to_latex(a.elements[a.numColumns *% row +% column])
      );
    let body = _joined_elements(a.numRows, " \\\\\n", create_row);
    "\\begin{bmatrix}\n" ++ body ++ "\n\\end{bmatrix}";
  };
};
