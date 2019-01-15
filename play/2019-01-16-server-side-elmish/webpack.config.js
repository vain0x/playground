const path = require("path")
const distDir = path.resolve(__dirname, "dist")

module.exports = {
  entry: [
    path.resolve(__dirname, "src/client/index.ts"),
  ],
  output: {
    filename: "index.js",
    path: path.join(distDir, "scripts"),
  },
  resolve: {
    extensions: [".ts", ".tsx", ".js", ".jsx"],
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: "ts-loader"
      },
    ],
  },
}
