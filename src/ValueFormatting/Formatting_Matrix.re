type matrixFormat = {
  matrixOpen: string,
  matrixClose: string,
  rowOpen: string,
  rowClose: string,
  rowSeparator: string,
  elementSeparator: string,
};

let formatString = {
  matrixOpen: "{",
  matrixClose: "}",
  rowOpen: "{",
  rowClose: "}",
  rowSeparator: ", ",
  elementSeparator: ", ",
};

let formatTex = {
  matrixOpen: "\\begin{bmatrix}\n",
  matrixClose: "\n\\end{bmatrix}",
  rowOpen: "",
  rowClose: "",
  rowSeparator: "\\\\\n",
  elementSeparator: " && ",
};

let formatMathML = {
  matrixOpen: "<mrow><mo>[</mo><mtable>",
  matrixClose: "</mtable><mo>]</mo></mrow>",
  rowOpen: "<mtr><mtd>",
  rowClose: "</mtd></mtr>",
  rowSeparator: "",
  elementSeparator: "</mtd><mtd>",
};

let toString = (matrix: Matrix.t, format, tableFormat) => {
  let out = ref(tableFormat.matrixOpen);

  for (row in 0 to matrix.numRows - 1) {
    if (row != 0) {
      out := out^ ++ tableFormat.rowSeparator;
    };

    out := out^ ++ tableFormat.rowOpen;

    for (column in 0 to matrix.numColumns - 1) {
      if (column != 0) {
        out := out^ ++ tableFormat.elementSeparator;
      };

      let element =
        Matrix.getExn(matrix, ~row, ~column)
        ->Formatting_Scalar.toString(_, format);
      out := out^ ++ element;
    };

    out := out^ ++ tableFormat.rowClose;
  };

  out := out^ ++ tableFormat.matrixClose;

  out^;
};
