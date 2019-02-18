module Make = (WithKey: {
                 let key: string;
                 type t;
               }) => {
  exception InvalidType;

  [@bs.deriving abstract]
  type wrappedValue = {
    _typeof: string,
    value: WithKey.t,
  };

  let unwrap = wrapper =>
    if (_typeofGet(wrapper) == WithKey.key) {
      valueGet(wrapper);
    } else {
      raise(InvalidType);
    };

  let wrap = value => wrappedValue(~_typeof=WithKey.key, ~value);
};
