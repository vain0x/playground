import { BatchApp, BatchCommand } from "./batch-app-framework";
import { googleCommand } from "./commands/google-command";
import { regexpCommand } from "./commands/regexp-command";
import { twitterCommand } from "./commands/twitter-command";

const commands: BatchCommand[] = [
  {
    verb: "help",
    alias: ["-h", "--help"],
    help() {
      return ["Print usage."];
    },
    async run(args, context) {
      context.printUsage();
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
  twitterCommand,
];

export const main = async () => {
  return await BatchApp.create(commands).main();
};
