let toQ = (q, c) => Constant.(c == Unit) ? q : Q.(q * Constant.toQ(c));

let toFloat = (q, c) =>
  Constant.(c == Unit) ? Q.to_float(q) : Q.(q * Constant.toQ(c))->Q.to_float;

let mapFloat = (q, c, f) =>
  Q.(q * Constant.toQ(c))->Q.to_float->f->Q.of_float;
