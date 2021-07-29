/* eslint-disable no-undef */

module.exports = {
  root: true,
  parser: "@typescript-eslint/parser",
  plugins: [
    "@typescript-eslint",
  ],
  extends: [
    "eslint:recommended",
    "plugin:@typescript-eslint/eslint-recommended",
    "plugin:@typescript-eslint/recommended",
  ],
  rules: {
    "comma-dangle": [
      "warn",
      "always-multiline",
    ],
    "semi": [
      "warn",
      "never",
    ],
    "@typescript-eslint/explicit-function-return-type": 0,
    "@typescript-eslint/explicit-module-boundary-types": 0,
    "@typescript-eslint/no-empty-function": "warn",
    "@typescript-eslint/no-empty-interface": "warn",
    "@typescript-eslint/no-explicit-any": 0,
    "@typescript-eslint/no-non-null-assertion": 0,
    "@typescript-eslint/no-unused-vars": 0,
    "no-constant-condition": 0,
    "prefer-const": "warn",
  },
}
