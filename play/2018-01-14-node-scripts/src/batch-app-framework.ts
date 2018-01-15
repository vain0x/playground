import * as os from "os";
import * as io from "./io";
import "./collection";

const pair = <T1, T2>(x1: T1, x2: T2): [T1, T2] => [x1, x2];

export type BatchCommand = {
  verb: string,
  alias?: string[] | undefined,
  help?: (() => string[]) | undefined,
  run(args: string[], app: BatchApp): Promise<void>,
};

export class BatchApp {
  private readonly commandFromVerb: Map<string, BatchCommand>;

  constructor(private readonly commands: BatchCommand[]) {
    const entries =
      commands.flatMap(c =>
        [c.verb, ...c.alias || []].map(verb => pair(c.verb, c))
      );
    this.commandFromVerb = new Map(entries);
  }

  usage() {
    const verbs =
      this.commands.map(c => c.verb);
    const helps =
      this.commands.choose(c => {
        if (c.help === undefined) return undefined;
        const help = c.help();
        if (help.length === 0) return undefined;
        return { command: c, help };
      });

    const lines = [
      "Verbs:",
      ...verbs.map(verb => `  ${verb}`),

      "Helps:",
      ...helps.flatMap(({ command, help }) => [
        `  ${command.verb}:`,
        ...help.map(line => `    ${line}`),
        "",
      ]),
    ];

    return lines;
  }

  printUsage() {
    console.info(this.usage().join(os.EOL));
  }

  async argumentList() {
    const [_0, _1, ...args] = process.argv;

    if (args.length === 0) {
      this.printUsage();
      console.log("Input argument:");
      const line = await io.readLine();
      return line.split(" ").filter(s => s !== "");
    }

    return args;
  }

  async execute() {
    const [verb, ...args] = await this.argumentList();

    const command = this.commandFromVerb.get(verb);
    if (command === undefined) {
      console.error(`Unknown verb '${verb}'.`);
      this.printUsage();
      return;
    }

    await command.run(args, this);
  }

  async main() {
    try {
      return await this.execute();
    } catch (ex) {
      console.error(ex);
      process.exit(1);
    }

    console.debug("End of main.");
  }
}
