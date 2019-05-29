open Factorial;

let nPr = (n, r) => Base.(n->factorial / (n - r)->factorial);

let nCr = (n, r) =>
  Base.(n->factorial / (r->factorial * (n - r)->factorial));
