open Factorial;

let nPr = (n, r) => BasicMath.(n->factorial / (n - r)->factorial);

let nCr = (n, r) =>
  BasicMath.(n->factorial / (r->factorial * (n - r)->factorial));
