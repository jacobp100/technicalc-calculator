type matrixFormat = {
  matrixOpen: string,
  matrixClose: string,
  rowOpen: string,
  rowClose: string,
  rowSeparator: string,
  elementSeparator: string,
};

let matrixFormatString = {
  matrixOpen: "{",
  matrixClose: "}",
  rowOpen: "{",
  rowClose: "}",
  rowSeparator: ", ",
  elementSeparator: ", ",
};

let matrixFormatTex = {
  matrixOpen: "\\begin{bmatrix}\n",
  matrixClose: "\n\\end{bmatrix}",
  rowOpen: "",
  rowClose: "",
  rowSeparator: "\\\\\n",
  elementSeparator: " && ",
};

let matrixFormatMathML = {
  matrixOpen: "<mrow><mo>[</mo><mtable>",
  matrixClose: "</mtable><mo>]</mo></mrow>",
  rowOpen: "<mtr><mtd>",
  rowClose: "</mtd></mtr>",
  rowSeparator: "",
  elementSeparator: "</mtd><mtd>",
};

let formatMatrix = ({Matrix.numRows, numColumns} as m, f) => {
  let out = ref(f.matrixOpen);

  for (row in 0 to numRows - 1) {
    if (row != 0) {
      out := out^ ++ f.rowSeparator;
    };

    out := out^ ++ f.rowOpen;

    for (column in 0 to numColumns - 1) {
      if (column != 0) {
        out := out^ ++ f.elementSeparator;
      };
      out := out^ ++ Matrix.getExn(m, row, column);
    };

    out := out^ ++ f.rowClose;
  };

  out := out^ ++ f.matrixClose;

  out^;
};
