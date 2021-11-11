/* eslint-disable @typescript-eslint/no-var-requires */
/* eslint-disable no-undef */

const path = require("path")

const STATIC_DIR = path.resolve(__dirname, "dist/static")
const TSCONFIG_PATH = path.resolve(__dirname, "tsconfig-client.json")

module.exports = {
  entry: "./src/client/index.tsx",

  output: {
    filename: "bundle.js",
    path: STATIC_DIR,
  },

  resolve: {
    extensions: [
      ".ts",
      ".tsx",
      ".js",
    ],
  },

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: "ts-loader",
        options: {
          configFile: TSCONFIG_PATH,
        },
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
}
