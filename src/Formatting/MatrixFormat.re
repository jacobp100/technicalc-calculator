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

let row1 = (a, f) => f.rowOpen ++ a ++ f.rowClose;
let row2 = (a, b, f) =>
  f.rowOpen ++ a ++ f.elementSeparator ++ b ++ f.rowClose;
let row3 = (a, b, c, f) =>
  f.rowOpen
  ++ a
  ++ f.elementSeparator
  ++ b
  ++ f.elementSeparator
  ++ c
  ++ f.rowClose;

let rows2 = (a, b, f) =>
  f.matrixOpen ++ a ++ f.rowSeparator ++ b ++ f.matrixClose;
let rows3 = (a, b, c, f) =>
  f.matrixOpen
  ++ a
  ++ f.rowSeparator
  ++ b
  ++ f.rowSeparator
  ++ c
  ++ f.matrixClose;
