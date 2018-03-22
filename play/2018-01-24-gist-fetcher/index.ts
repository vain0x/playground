import dotenv from "dotenv";
import { fetchAllGists } from "github-api";

function forceCast(type: "undefined", value: any): undefined;
function forceCast(type: "string", value: any): string;
function forceCast(type: "number", value: any): number;
function forceCast(type: "object", value: any): object;
function forceCast(type: any, value: any) {
  if (typeof value !== type) {
    throw new Error(`Expected a ${typeof type} but ${value}.`);
  }
  return value;
}

const main = async () => {
  const { error } = dotenv.config();
  if (error) throw error;

  const githubAuth = {
    userName:
      forceCast("string", process.env.GITHUB_USER_NAME),
    password:
      forceCast("string", process.env.GITHUB_PERSONAL_ACCESS_TOKEN),
  };

  await fetchAllGists({ githubAuth });
  // createGist();
};

main();
