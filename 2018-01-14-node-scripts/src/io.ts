import * as readline from "readline";

/**
 * Reads a line from standard input.
 */
export const readLine = () => {
  return new Promise<string>((resolve, _) => {
    const reader = readline.createInterface({
      input: process.stdin,
    });

    reader.once("line", input => {
      reader.close();
      resolve(input);
    });
  });
};
