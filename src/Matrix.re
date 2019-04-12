let _elementIndex = (~numColumns, row, column) => column + row * numColumns;

let _joinedElements = (elements, join, fn) =>
  String.concat(join, Belt.List.fromArray(Belt.Array.makeBy(elements, fn)));

module Make = (Number: Types.Scalar) => {
  open PervasivesNoPoly;
  open PervasivesMath;

  let (==) = Number.equal;
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

  let toString = (~format=OutputFormat.default, a) => {
    let elementString = (row, column) =>
      Number.toString(~format, a.elements[a.numColumns *% row +% column]);

    switch (format.mode) {
    | String =>
      let bracketedElements = (elements, fn) =>
        "{" ++ _joinedElements(elements, ", ", fn) ++ "}";
      let createRow = row =>
        bracketedElements(a.numColumns, column => elementString(row, column));
      bracketedElements(a.numRows, createRow);
    | Tex =>
      let createRow = row =>
        _joinedElements(a.numColumns, " && ", column =>
          elementString(row, column)
        );
      let body = _joinedElements(a.numRows, " \\\\\n", createRow);
      "\\begin{bmatrix}\n" ++ body ++ "\n\\end{bmatrix}";
    | MathML =>
      let createRow = row =>
        "<mtr>"
        ++ _joinedElements(a.numColumns, "", column =>
             "<mtd>" ++ elementString(row, column) ++ "</mtd>"
           )
        ++ "</mtr>";
      let body = _joinedElements(a.numRows, "", createRow);
      "<mrow><mtable>" ++ body ++ "</mtable></mrow>";
    };
  };

  let nan = {numRows: 0, numColumns: 0, elements: [||]};
  let isNan = a => Belt.Array.length(a.elements) ==% 0;

  let shapeEqual = (a, b) =>
    a.numRows ==% b.numRows && a.numColumns ==% b.numColumns;

  let equal = (a, b) =>
    if (shapeEqual(a, b)) {
      Belt.Array.reduceWithIndex(a.elements, true, (match, aElem, i) =>
        match && aElem == b.elements[i]
      );
    } else {
      false;
    };

  let normalize = a =>
    Belt.Array.reduce(a.elements, a, (current, e) =>
      Number.isNan(e) ? nan : current
    );

  let fromElements = (~rows as numRows, ~columns as numColumns, elements) =>
    normalize({numRows, numColumns, elements});
  let fromMatrix = elements => {
    let numRows = Belt.Array.length(elements);
    if (numRows >% 0) {
      let numColumns = Belt.Array.length(elements[0]);
      let elements = Belt.Array.concatMany(elements);
      normalize({numRows, numColumns, elements});
    } else {
      nan;
    };
  };
  let init = (~rows as numRows, ~columns as numColumns, fn) => {
    let elements =
      Belt.Array.makeBy(
        numRows *% numColumns,
        elementIndex => {
          let column = elementIndex mod numColumns;
          let row = elementIndex /% numColumns;
          fn(row, column);
        },
      );
    normalize({numRows, numColumns, elements});
  };
  let fromIdentity = (~rows, ~columns) =>
    init(~rows, ~columns, (row, column) =>
      row ==% column ? Number.one : Number.zero
    );
  let toElements = a => a.elements;
  let toMatrix = ({numRows, numColumns, elements}) => {
    Belt.Array.makeBy(numRows, row =>
      Belt.Array.makeBy(numColumns, column =>
        elements[_elementIndex(~numColumns, row, column)]
      )
    );
  };

  let _combine = (iter, a, b) =>
    if (shapeEqual(a, b)) {
      let elements =
        a.elements
        ->Belt.Array.mapWithIndex((i, aElem) => iter(aElem, b.elements[i]));
      normalize({...a, elements});
    } else {
      nan;
    };

  let add = _combine(Number.add);
  let sub = _combine(Number.sub);

  let _mapElements = (iter, a) =>
    normalize({...a, elements: Belt.Array.map(a.elements, iter)});

  let mulConst = (a, c) => _mapElements(Number.mul(c), a);
  let divConst = (a, c) => _mapElements(v => Number.div(v, c), a);

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
            let aIndex = _elementIndex(~numColumns=shape, row, i);
            let bIndex = _elementIndex(~numColumns=shape, i, column);
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
      divConst({numRows: 2, numColumns: 2, elements}, factor);
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
      divConst({numRows: 3, numColumns: 3, elements}, factor);
    | _ => nan
    };

  let powConst = (a, c) => {
    switch (Number.toInt(c)) {
    | Some((-1)) => inverse(a)
    | Some(0) => fromIdentity(~rows=a.numRows, ~columns=a.numColumns)
    | Some(1) => a
    | Some(pow) when pow <% 20 && pow >% 0 =>
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
    | ({numColumns: 1, elements: aElems}, {numColumns: 1, elements: bElems})
        when a.numRows ==% b.numRows =>
      Belt.Array.reduceWithIndex(aElems, Number.zero, (accum, aElem, i) =>
        accum + aElem * bElems[i]
      )
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

  let neg = _mapElements(Number.neg);
};
