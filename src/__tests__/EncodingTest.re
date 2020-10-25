open Jest;
open Encoding;

test("integers", (.) => {
  for (value in 0 to 4096) {
    let encoded = encodeInt(value);

    let decoded = read(encoded, readInt);
    expect(decoded)->toEqual(Some(value));
  }
});

test("strings", (.) => {
  let string = "Hello world !@#$%^&*()";
  let encoded = encodeString(string);
  let decoded = read(encoded, readString);
  expect(decoded)->toEqual(Some(string));
});

test("arrays", (.) => {
  let array = [|1, 2, 3|];
  let encoded = encodeArray(array, (. value) => {encodeInt(value)});
  let decoded =
    read(encoded, reader => {
      readArray(reader, (. reader) => {readInt(reader)})
    });
  expect(decoded)->toEqual(Some(array));
});
