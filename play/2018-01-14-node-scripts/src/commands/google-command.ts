import * as os from "os";
import * as io from "../io";
import { URL, URLSearchParams } from "url";
import { BatchCommand } from "../batch-app-framework";

const opn = require("opn") as (target: string, option: {}) => Promise<any>;

const googleSearchURI = (q: string) => {
  const uri = new URL("http://google.com/search");
  uri.search = new URLSearchParams({ q }).toString();
  return uri;
};

export const googleCommand: BatchCommand = {
  verb: "google",
  help: () => [
    "google ...<word>",
    "Googles specified words.",
  ],
  async run(args) {
    let query: string | undefined = undefined;
    if (args.length === 0) {
      query = await io.readLine();
    } else {
      // Because shell splits command line argument by spaces,
      // words including any space are quoted by user.
      query = args.map(word => word.includes(" ") ? `"${word}"` : word).join(" ");
    }
    const uri = googleSearchURI(query);

    await opn(uri.toString(), { wait: false });
  }
};
