import * as os from "os";
import * as io from "../io";
import * as fs from "fs";
import * as path from "path";
import * as tmp from "tmp";
import * as child_process from "child_process";
import { BatchCommand } from "../batch-app-framework";

import Twitter = require("node-twitter-api");

interface TwitterConfig {
  consumer_key: string;
  consumer_secret: string;
  access_token_key: string;
  access_token_secret: string;
}

const readByCode = (content: string) => new Promise<string>((resolve, reject) => {
  tmp.file((err, temporaryFilePath) => {
    fs.writeFile(temporaryFilePath, content, err => {
      if (err) return reject(err);

      child_process.exec(`code --wait ${temporaryFilePath}`, (err, stdout, stderr) => {
        if (err) return reject(err);

        fs.readFile(temporaryFilePath, (err, data) => {
          if (err) return reject(err);
          return resolve(data.toString());
        });
      });
    });
  });
});

const createTwitterClient = (config: {
  consumer_key: string,
  consumer_secret: string,
}) => {
  return new Twitter({
    consumerKey: config.consumer_key,
    consumerSecret: config.consumer_secret,
  });
};

const tweet = (status: string, twitterClient: Twitter, config: TwitterConfig) => new Promise<void>((resolve, reject) => {
  twitterClient.statuses("update", { status },
    config.access_token_key,
    config.access_token_secret,
    (err: any) => {
      if (err) return reject(err);
      resolve();
    });
});

export const twitterCommand: BatchCommand = {
  verb: "twitter:new",
  help: () => [
    "twitter:new",
    "Submits a tweet.",
  ],
  async run(args, context) {
    const config = await context.configRepo.make<TwitterConfig>("twitter").load();
    if (config === undefined) throw new Error("Twitter config doesn't exist.");

    const twitterClient = createTwitterClient(config);

    let content = "";

    while (true) {
      content = (await readByCode(content)).trim();
      if (content === "") {
        console.warn("Canceled due to empty content.");
        return;
      }

      try {
        await tweet(content, twitterClient, config);
      } catch (ex) {
        console.warn("Error occured while submitting:");
        console.warn(ex);
        continue;
      }

      break;
    }

    console.warn("OK.");
  }
};
