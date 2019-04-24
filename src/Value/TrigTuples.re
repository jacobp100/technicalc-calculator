/*
 Exp depends on some definition of sin/cos
 In turn, sin/cos depend of a definition of exp
 */

let sinReal = (q, c) =>
  switch (TrigUtil.compareTrig(q, c)) {
  | (0, 1, _)
  | (1 | 2, 1, `Pi) => (Q.zero, Constant.Unit)
  | (1, 2, `Pi) => (Q.one, Unit)
  | (3, 2, `Pi) => (Q.minus_one, Unit)
  | (1 | 2, 3, `Pi) => (Q.of_ints(1, 2), Sqrt(Z.of_int(3)))
  | (4 | 5, 3, `Pi) => (Q.of_ints(-1, 2), Sqrt(Z.of_int(3)))
  | (1 | 3, 4, `Pi) => (Q.of_ints(1, 2), Sqrt(Z.of_int(2)))
  | (5 | 7, 4, `Pi) => (Q.of_ints(-1, 2), Sqrt(Z.of_int(2)))
  | (1 | 5, 6, `Pi) => (Q.of_ints(1, 2), Unit)
  | (7 | 11, 6, `Pi) => (Q.of_ints(-1, 2), Unit)
  | _ => (QCUtil.mapFloat(q, c, sin), Unit)
  };

let cosReal = (q, c) =>
  switch (TrigUtil.compareTrig(q, c)) {
  | (0, 1, _) => (Q.one, Constant.Unit)
  | (2, 1, `Pi) => (Q.one, Unit)
  | (1, 1, `Pi) => (Q.minus_one, Unit)
  | (1 | 3, 2, `Pi) => (Q.zero, Unit)
  | (1 | 5, 3, `Pi) => (Q.of_ints(1, 2), Unit)
  | (2 | 4, 3, `Pi) => (Q.of_ints(-1, 2), Unit)
  | (1 | 7, 4, `Pi) => (Q.of_ints(1, 2), Sqrt(Z.of_int(2)))
  | (3 | 5, 4, `Pi) => (Q.of_ints(-1, 2), Sqrt(Z.of_int(2)))
  | (1 | 11, 6, `Pi) => (Q.of_ints(1, 2), Sqrt(Z.of_int(3)))
  | (5 | 7, 6, `Pi) => (Q.of_ints(-1, 2), Sqrt(Z.of_int(3)))
  | _ => (QCUtil.mapFloat(q, c, cos), Unit)
  };
