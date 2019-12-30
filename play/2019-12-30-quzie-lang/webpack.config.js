// webpack の設定

const path = require("path")

module.exports = {
  context: __dirname,

  entry: {
    main: path.resolve("src/index.js"),
  },

  output: {
    globalObject: "self",
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "dist"),
  },

  // webpack-dev-server の設定
  devServer: {
    contentBase: "dist",
    compress: true,
    port: 8080,
  },
}
