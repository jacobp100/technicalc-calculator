module.exports =
  process.env.NODE_ENV === "test"
    ? {
        presets: [
          [
            "@babel/preset-env",
            { loose: true, modules: "commonjs", targets: { node: "current" } }
          ]
        ]
      }
    : {};
