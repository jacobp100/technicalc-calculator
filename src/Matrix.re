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

  let _element_index = (~numColumns, row, column) =>
    column +% row *% numColumns;

  let nan = {numRows: 0, numColumns: 0, elements: [||]};
  let is_nan = a => Array.length(a.elements) ==% 0;

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

  let from_elements = (~rows as numRows, ~columns as numColumns, elements) =>
    normalize({numRows, numColumns, elements});
  let from_matrix = elements => {
    let numRows = Array.length(elements);
    if (numRows > 0) {
      let numColumns = Array.length(elements[0]);
      let elements = Array.concat(Array.to_list(elements));
      normalize({numRows, numColumns, elements});
    } else {
      nan;
    };
  };
  let init = (~rows as numRows, ~columns as numColumns, fn) => {
    let elements =
      Array.init(
        numRows *% numColumns,
        elementIndex => {
          let column = elementIndex mod numColumns;
          let row = elementIndex /% numColumns;
          fn(row, column);
        },
      );
    normalize({numRows, numColumns, elements});
  };
  let from_identity = (~rows, ~columns) =>
    init(~rows, ~columns, (row, column) =>
      row ==% column ? Number.one : Number.zero
    );
  let to_elements = a => a.elements;
  let to_matrix = ({numRows, numColumns, elements}) => {
    Array.init(numRows, row =>
      Array.init(numColumns, column =>
        elements[_element_index(~numColumns, row, column)]
      )
    );
  };

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

  let add = _combine(Number.add);
  let sub = _combine(Number.sub);

  let _map_elements = (iter, a) =>
    normalize({...a, elements: Array.map(iter, a.elements)});

  let mul_const = (a, c) => _map_elements(Number.mul(c), a);
  let div_const = (a, c) => _map_elements(v => Number.div(v, c), a);

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
      init(
        ~rows=shape,
        ~columns=shape,
        (row, column) => {
          let element = ref(Number.zero);
          for (i in 0 to shape -% 1) {
            let aIndex = _element_index(~numColumns=shape, row, i);
            let bIndex = _element_index(~numColumns=shape, i, column);
            element := element^ + a.elements[aIndex] * b.elements[bIndex];
          };
          element^;
        },
      );
    | _ => nan
    };

  let inverse = a =>
    switch (a) {
    | {numRows: 2, numColumns: 2, elements: [|a, b, c, d|]} =>
      let factor = a * d - b * c;
      let elements = [|d, - b, - c, a|];
      div_const({numRows: 2, numColumns: 2, elements}, factor);
    | {numRows: 3, numColumns: 3, elements: [|a, b, c, d, e, f, g, h, i|]} =>
      /* https://www.wolframalpha.com/input/?i=%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D%5E-1 */
      let factor =
        a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g;
      let elements = [|
        e * i - f * h,
        c * h - b * i,
        b * f - c * e,
        f * g - d * i,
        a * i - c * g,
        c * d - a * f,
        d * h - e * g,
        b * g - a * h,
        a * e - b * d,
      |];
      div_const({numRows: 3, numColumns: 3, elements}, factor);
    | _ => nan
    };

  let pow_const = (a, c) => {
    switch (Number.to_int(c)) {
    | Some((-1)) => inverse(a)
    | Some(0) => from_identity(~rows=a.numRows, ~columns=a.numColumns)
    | Some(1) => a
    | Some(pow) when pow < 20 && pow > 0 =>
      let m = ref(a);
      for (_ in 2 to pow) {
        m := mul(m^, m^);
      };
      m^;
    | _ => nan
    };
  };

  let dot = (a, b) =>
    switch (a, b) {
    | ({numColumns: 1, elements: aElem}, {numColumns: 1, elements: bElem})
        when a.numRows ==% b.numRows =>
      let accum = ref(Number.zero);
      Array.iteri((i, _) => accum := accum^ + aElem[i] * bElem[i], aElem);
      accum^;
    | _ => Number.nan
    };

  let det = a =>
    switch (a) {
    | {numColumns: 1} => dot(a, a)
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
