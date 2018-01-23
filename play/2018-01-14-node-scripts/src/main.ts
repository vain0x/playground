import { BatchApp, BatchCommand } from "./batch-app-framework";
import { googleCommand } from "./commands/google-command";
import { regexpCommand } from "./commands/regexp-command";

const commands: BatchCommand[] = [
  {
    verb: "help",
    alias: ["-h", "--help"],
    help() {
      return ["Print usage."];
    },
    async run(args, app) {
      app.printUsage();
    },
  },
  {
    verb: "hello",
    async run(args) {
      console.log("Hello, world!");
    },
  },
  googleCommand,
  regexpCommand,
];

export const main = async () => {
  return await new BatchApp(commands).main();
};
