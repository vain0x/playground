// webpack の設定
// 参考: https://github.com/microsoft/monaco-editor-samples/tree/master/browser-esm-webpack-typescript

const path = require("path")

module.exports = {
  context: __dirname,

  entry: {
    main: "./src/index.tsx",
    "editor.worker": "monaco-editor/esm/vs/editor/editor.worker.js",
    "json.worker": "monaco-editor/esm/vs/language/json/json.worker",
    "css.worker": "monaco-editor/esm/vs/language/css/css.worker",
    "html.worker": "monaco-editor/esm/vs/language/html/html.worker",
    "ts.worker": "monaco-editor/esm/vs/language/typescript/ts.worker",
  },

  output: {
    globalObject: "self",
    filename: "[name].bundle.js",
    path: path.join(__dirname, "dist"),
  },

  resolve: {
    extensions: [".ts", ".tsx", ".js", ".jsx"],
  },

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: "ts-loader",
      },

      // monaco-editorには必須。
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.ttf$/,
        use: ["file-loader"]
      },

      {
        enforce: "pre",
        test: /\.js$/,
        loader: "source-map-loader",
      },
    ],
  },

  externals: {
    "react": "React",
    "react-dom": "ReactDOM",
  },

  devServer: {
    compress: true,
    contentBase: "dist",
  },
}
