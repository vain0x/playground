const dotenv = require("dotenv");
const passport = require("passport");
const OAuth2Strategy = require("passport-oauth2");
const { URL, URLSearchParams } = require("url");
const https = require("https");

dotenv.config();

const authInfo = {
  userName: process.env.GITHUB_USER_NAME,
  accessToken: process.env.GITHUB_PERSONAL_ACCESS_TOKEN,
};

const sendRequest = (path, method, query, contentType, content) => new Promise((resolve, reject) => {
  const authorization = "token " + authInfo.accessToken;
  const host = "api.github.com";

  const headers = {
    "User-Agent": "my-gist-fetcher",
    "Authorization": authorization,
    "Accept": "application/vnd.github.v3+json",
  };

  if (content.length > 0) {
    headers["Content-Type"] = contentType;
    headers["Content-Length"] = content.length;
  }

  const param = new URLSearchParams(query);
  const pathWithQuery = (() => {
    if (Array.from(param.entries()).length === 0) return path;
    return path + "?" + param.toString();
  })();

  const requestOption = { host, path: pathWithQuery, method, headers };
  console.warn(requestOption);

  const request = https.request(requestOption, response => {
    const chunks = [];

    console.warn('RESPONSE STATUS: ' + response.statusCode);
    console.warn('RESPONSE HEADERS: ' + JSON.stringify(response.headers));
    response.setEncoding('utf8');
    response.on('data', (chunk) => {
      console.warn('BODY: ' + chunk);
      chunks.push(chunk);
    });
    response.on("error", error => {
      reject(error);
    });
    response.on("end", () => {
      resolve(chunks.join(""));
    });
  });

  request.on("error", error => {
    reject(error);
  });

  if (content.length > 0) {
    request.write(content);
    request.end();
  }
});

const sendGetRequest = (path, query, contentType, content) => {
  return sendRequest(path, "GET", query, contentType, content);
};

const sendPostRequest = (path, query, contentType, content) => {
  return sendRequest(path, "POST", query, contentType, content);
};

const fetchAllGists = async () => {
  const fetchSinglePageOfGistList = async pageIndex => {
    const userName = authInfo.userName;
    // const since = "2018-01-01T00:00:00Z";
    const result = await sendGetRequest(`/users/${userName}/gists`, { page: pageIndex }, "application/json", "{}");
    const repos = JSON.parse(result);
    const summaries = repos.map(repo => ({
      id: repo["id"],
      createdAt: new Date(repo["created_at"]),
      name: (() => {
        const keys = Object.keys(repo["files"]);
        return keys.length === 0 ? "_empty" : repo["files"][keys[0]]["filename"];
      })(),
      pullURI: repo["git_pull_url"],
    }));
    return summaries;
  };

  const summaries = [];
  for (let i = 0; i < 100000; i++) {
    const page = await fetchSinglePageOfGistList(i);
    if (page.length === 0) break;

    for (const x of page) {
      summaries.push(x);
    }
  }

  console.log(JSON.stringify(summaries, undefined, "  "));
};

const createGist = async () => {
  const result = await sendPostRequest("/gists", {}, "application/json", JSON.stringify({
    files: {
      "file1.txt": {
        content: "Hello, world!",
      },
    },
    description: "Automatically created via API."
  }));
  console.log("completed");
  console.log(result);
};

fetchAllGists();
// createGist();
