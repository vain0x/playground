import { BatchApp, BatchCommand } from "./batch-app-framework";

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
];

export const main = async () => {
  return await new BatchApp(commands).main();
};
