module Make = (V: Types.BaseValue) => {
  type iterRange('t) = {
    start: 't,
    upTo: 't,
    body: 't,
  };

  type t =
    | Value(V.t)
    | Encoded(V.encoding)
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
    | Asin(t)
    | Sinh(t)
    | Asinh(t)
    | Cos(t)
    | Acos(t)
    | Cosh(t)
    | Acosh(t)
    | Tan(t)
    | Atan(t)
    | Tanh(t)
    | Atanh(t)
    | Factorial(t)
    | Sum(iterRange(t))
    | Product(iterRange(t));

  module Context = Map.Make(String);

  let ofInt = a => Value(V.ofInt(a));
  let ofFloat = a => Value(V.ofFloat(a));
  let ofString = a => Value(V.ofString(a));
  let ofStringBase = (base, a) => Value(V.ofStringBase(base, a));
  let ofEncoded = a => Encoded(a);
  let ofT = a => Value(a);
  let matrixOfElements = (rows, columns, elements) =>
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
  let asin = a => Asin(a);
  let sinh = a => Sinh(a);
  let asinh = a => Asinh(a);
  let cos = a => Cos(a);
  let acos = a => Acos(a);
  let cosh = a => Cosh(a);
  let acosh = a => Acosh(a);
  let tan = a => Tan(a);
  let atan = a => Atan(a);
  let tanh = a => Tanh(a);
  let atanh = a => Atanh(a);
  let factorial = a => Factorial(a);
  let sum = (s, t, b) => Sum({start: s, upTo: t, body: b});
  let product = (s, t, b) => Product({start: s, upTo: t, body: b});

  let rec eval = (~context=Context.empty, node: t): V.t =>
    switch (node) {
    | Value(a) => a
    | Encoded(a) => V.decode(a)
    | Matrix(rows, columns, elements) =>
      elements
      ->Belt.Array.map(eval(~context))
      ->V.ofMatrixElements(~rows, ~columns)
    | Variable(ident) =>
      switch (Context.find(ident, context)) {
      | v => v
      | exception _ => V.nan
      }
    | Add(a, b) => V.add(eval(~context, a), eval(~context, b))
    | Sub(a, b) => V.sub(eval(~context, a), eval(~context, b))
    | Mul(a, b) => V.mul(eval(~context, a), eval(~context, b))
    | Div(a, b) => V.div(eval(~context, a), eval(~context, b))
    | Pow(a, b) => V.pow(eval(~context, a), eval(~context, b))
    | Dot(a, b) => V.dot(eval(~context, a), eval(~context, b))
    | Neg(a) => V.neg(eval(~context, a))
    | Abs(a) => V.abs(eval(~context, a))
    | Sqrt(a) => V.sqrt(eval(~context, a))
    | Exp(a) => V.exp(eval(~context, a))
    | Log(a) => V.log(eval(~context, a))
    | Factorial(a) => V.factorial(eval(~context, a))
    | Sin(a) => V.sin(eval(~context, a))
    | Asin(a) => V.asin(eval(~context, a))
    | Sinh(a) => V.sinh(eval(~context, a))
    | Asinh(a) => V.asinh(eval(~context, a))
    | Cos(a) => V.cos(eval(~context, a))
    | Acos(a) => V.acos(eval(~context, a))
    | Cosh(a) => V.cosh(eval(~context, a))
    | Acosh(a) => V.acosh(eval(~context, a))
    | Tan(a) => V.tan(eval(~context, a))
    | Atan(a) => V.atan(eval(~context, a))
    | Tanh(a) => V.tanh(eval(~context, a))
    | Atanh(a) => V.atanh(eval(~context, a))
    | Sum(range) => reduceRange(~context, V.add, V.zero, range)
    | Product(range) => reduceRange(~context, V.mul, V.one, range)
    }
  and reduceRange = (~context=Context.empty, iteratee, initialValue, range) => {
    let startVal = eval(~context, range.start);
    let upToVal = eval(~context, range.upTo);
    switch (V.toInt(startVal), V.toInt(upToVal)) {
    | (Some(f), Some(t)) =>
      let current = ref(initialValue);
      for (v in f to t) {
        let nextContextValue = V.ofInt(v);
        let nextContext = Context.add("x", nextContextValue, context);
        let nextValue = eval(~context=nextContext, range.body);
        current := iteratee(current^, nextValue);
      };
      current^;
    | _ => V.nan
    };
  };
};
