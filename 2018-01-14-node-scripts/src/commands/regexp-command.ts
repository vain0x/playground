import * as readline from "readline";
import * as os from "os";
import { BatchCommand } from "../batch-app-framework";

const replaceAll = (pattern: string, subst: string) => {
  const r = new RegExp(pattern, "g");

  const reader = readline.createInterface(process.stdin);
  reader.on("line", (line: string) => {
    process.stdout.write(line.replace(r, subst));
    process.stdout.write(os.EOL);
  });
};

export const regexpCommand: BatchCommand = {
  verb: "regexp",
  help: () => [
    "regexp <pattern> <subst>",
    "Searches all substrings that match the specified regexp pattern and replaces them. <subst> may contain $1 etc. for replacement of captured substrings.",
  ],
  async run(args) {
    if (args.length === 2) {
      const [pattern, subst] = args;
      replaceAll(pattern, subst);
    } else {
      throw new Error("Invalid number of arguments.");
    }
  }
};
