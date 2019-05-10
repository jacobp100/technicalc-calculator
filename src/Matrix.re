type t('a) = {
  numRows: int,
  numColumns: int,
  elements: array('a),
};

let makeBy = (numRows, numColumns, fn) => {
  numRows,
  numColumns,
  elements:
    Belt.Array.makeBy(
      numRows * numColumns,
      i => {
        let column = i mod numColumns;
        let row = i / numColumns;
        fn(row, column);
      },
    ),
};

let ofVector = elements => {
  numRows: Belt.Array.length(elements),
  numColumns: 1,
  elements,
};

let identity = size => makeBy(size, size, (a, b) => a == b ? 1 : 0);

let toNestedArrays = x =>
  Belt.Array.makeBy(x.numRows, row =>
    Belt.Array.slice(
      x.elements,
      ~offset=row * x.numColumns,
      ~len=x.numColumns,
    )
  );

let getUnsafe = (x, row, column) =>
  Belt.Array.getUnsafe(x.elements, column + row * x.numColumns);

let get = (x, row, column) =>
  if (row >= 0 && row <= x.numRows && column >= 0 && column <= x.numColumns) {
    getUnsafe(x, row, column);
  } else {
    None;
  };

let getExn = (x, row, column) =>
  if (row >= 0 && row <= x.numRows && column >= 0 && column <= x.numColumns) {
    Belt.Array.getUnsafe(x.elements, column + row * x.numColumns);
  } else {
    raise(Not_found);
  };

let map = (x, fn) => {...x, elements: Belt.Array.map(x.elements, fn)};

let zipBy = (x, y, fn) =>
  x.numRows == y.numRows && x.numColumns == y.numColumns ?
    Some({...x, elements: Belt.Array.zipBy(x.elements, y.elements, fn)}) :
    None;

let mapWithIndex = (x, fn) => {
  ...x,
  elements:
    Belt.Array.mapWithIndex(
      x.elements,
      (i, v) => {
        let column = i mod x.numColumns;
        let row = i / x.numColumns;
        fn(row, column, v);
      },
    ),
};
