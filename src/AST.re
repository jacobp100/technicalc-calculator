module Make = (V: Types.BaseValue) => {
  type iter_range('t) = {
    start: 't,
    upTo: 't,
    body: 't,
  };

  type t =
    | Value(V.t)
    | Matrix(int, int, array(t))
    | Variable(string)
    | Add(t, t)
    | Sub(t, t)
    | Mul(t, t)
    | Div(t, t)
    | Pow(t, t)
    | Dot(t, t)
    | Neg(t)
    | Abs(t)
    | Sqrt(t)
    | Exp(t)
    | Log(t)
    | Sin(t)
    | Arcsin(t)
    | Sinh(t)
    | Arcsinh(t)
    | Cos(t)
    | Arccos(t)
    | Cosh(t)
    | Arccosh(t)
    | Tan(t)
    | Arctan(t)
    | Tanh(t)
    | Arctanh(t)
    | Factorial(t)
    | Sum(iter_range(t))
    | Product(iter_range(t));

  module Context = Map.Make(String);

  let value_of_int = a => Value(V.of_int(a));
  let value_of_float = a => Value(V.of_float(a));
  let value_of_string = a => Value(V.of_string(a));
  let value_of_t = a => Value(a);
  let matrix_of_elements = (rows, columns, elements) =>
    Matrix(rows, columns, elements);
  let variable = name => Variable(name);
  let add = (a, b) => Add(a, b);
  let sub = (a, b) => Sub(a, b);
  let mul = (a, b) => Mul(a, b);
  let div = (a, b) => Div(a, b);
  let pow = (a, b) => Pow(a, b);
  let dot = (a, b) => Dot(a, b);
  let neg = a => Neg(a);
  let abs = a => Abs(a);
  let sqrt = a => Sqrt(a);
  let exp = a => Exp(a);
  let log = a => Log(a);
  let sin = a => Sin(a);
  let arcsin = a => Arcsin(a);
  let sinh = a => Sinh(a);
  let arcsinh = a => Arcsinh(a);
  let cos = a => Cos(a);
  let arccos = a => Arccos(a);
  let cosh = a => Cosh(a);
  let arccosh = a => Arccosh(a);
  let tan = a => Tan(a);
  let arctan = a => Arctan(a);
  let tanh = a => Tanh(a);
  let arctanh = a => Arctanh(a);
  let factorial = a => Factorial(a);
  let sum = (s, t, b) => Sum({start: s, upTo: t, body: b});
  let product = (s, t, b) => Product({start: s, upTo: t, body: b});

  let rec eval = (~context=Context.empty, node: t): V.t =>
    switch (node) {
    | Value(a) => a
    | Matrix(rows, columns, elements) =>
      let elements = Array.map(eval(~context), elements);
      V.of_matrix_elements(~rows, ~columns, elements);
    | Variable(ident) =>
      switch (Context.find(ident, context)) {
      | v => v
      | exception Not_found => V.nan
      }
    | Add(a, b) => V.add(eval(a), eval(b))
    | Sub(a, b) => V.sub(eval(a), eval(b))
    | Mul(a, b) => V.mul(eval(a), eval(b))
    | Div(a, b) => V.div(eval(a), eval(b))
    | Pow(a, b) => V.pow(eval(a), eval(b))
    | Dot(a, b) => V.dot(eval(a), eval(b))
    | Neg(a) => V.neg(eval(a))
    | Abs(a) => V.abs(eval(a))
    | Sqrt(a) => V.sqrt(eval(a))
    | Exp(a) => V.exp(eval(a))
    | Log(a) => V.log(eval(a))
    | Factorial(a) => V.factorial(eval(a))
    | Sin(a) => V.sin(eval(a))
    | Arcsin(a) => V.arcsin(eval(a))
    | Sinh(a) => V.sinh(eval(a))
    | Arcsinh(a) => V.arcsinh(eval(a))
    | Cos(a) => V.cos(eval(a))
    | Arccos(a) => V.arccos(eval(a))
    | Cosh(a) => V.cosh(eval(a))
    | Arccosh(a) => V.arccosh(eval(a))
    | Tan(a) => V.tan(eval(a))
    | Arctan(a) => V.arctan(eval(a))
    | Tanh(a) => V.tanh(eval(a))
    | Arctanh(a) => V.arctanh(eval(a))
    | Sum(range) => reduce_range(~context, V.add, V.zero, range)
    | Product(range) => reduce_range(~context, V.mul, V.one, range)
    }
  and reduce_range = (~context=Context.empty, iteratee, initialValue, range) => {
    let startVal = eval(~context, range.start);
    let upToVal = eval(~context, range.upTo);
    switch (V.to_int(startVal), V.to_int(upToVal)) {
    | (Some(f), Some(t)) =>
      let current = ref(initialValue);
      for (v in f to t) {
        let next_context_value = V.of_int(v);
        let next_context = Context.add("x", next_context_value, context);
        let next_value = eval(~context=next_context, range.body);
        current := iteratee(current^, next_value);
      };
      current^;
    | _ => V.nan
    };
  };
};
