type matrixFormat = {
  matrixOpen: string,
  matrixClose: string,
  rowOpen: string,
  rowClose: string,
  rowSeparator: string,
  elementOpen: string,
  elementClose: string,
  elementSeparator: string,
};

let matrixFormatString = {
  matrixOpen: "{",
  matrixClose: "}",
  rowOpen: "{",
  rowClose: "}",
  rowSeparator: ", ",
  elementOpen: "",
  elementClose: "",
  elementSeparator: ", ",
};

let matrixFormatTex = {
  matrixOpen: "\\begin{bmatrix}\n",
  matrixClose: "\n\\end{bmatrix}",
  rowOpen: "",
  rowClose: "",
  rowSeparator: "\\\\\n",
  elementOpen: "",
  elementClose: "",
  elementSeparator: " && ",
};

let matrixFormatMathML = {
  matrixOpen: "<mrow><mtable>",
  matrixClose: "</mtable></mrow>",
  rowOpen: "<mtr>",
  rowClose: "</mtr>",
  rowSeparator: "",
  elementOpen: "<mtd>",
  elementClose: "</mtd>",
  elementSeparator: "",
};

let row1 = (a, f) =>
  f.rowOpen ++ f.elementOpen ++ a ++ f.elementClose ++ f.rowClose;
let row2 = (a, b, f) =>
  f.rowOpen
  ++ f.elementOpen
  ++ a
  ++ f.elementSeparator
  ++ b
  ++ f.elementClose
  ++ f.rowClose;
let row3 = (a, b, c, f) =>
  f.rowOpen
  ++ f.elementOpen
  ++ a
  ++ f.elementSeparator
  ++ b
  ++ f.elementSeparator
  ++ c
  ++ f.elementClose
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
