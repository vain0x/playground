import { URL, URLSearchParams } from "url";
import https from "https";

interface GitHubAuth {
  userName: string;
  password: string;
}

interface APIRequest {
  path: string;
  query: [string, string][]; content?: { type: string, body: string };
  githubAuth: GitHubAuth;
}

interface GistMeta {
  id: string | number,
  created_at: string,
  git_pull_url: string,
  files: {
    [key: string]: {
      filename: string
    }
  }
}

const sendRequest = (r: APIRequest, method: "GET" | "POST") => new Promise<string>((resolve, reject) => {
  const authorization = "token " + r.githubAuth.password;
  const host = "api.github.com";

  const headers: { [key: string]: any } = {
    "User-Agent": "my-gist-fetcher",
    "Authorization": authorization,
    "Accept": "application/vnd.github.v3+json",
  };

  if (r.content !== undefined) {
    headers["Content-Type"] = r.content.type;
    headers["Content-Length"] = r.content.body.length;
  }

  const param = new URLSearchParams(r.query);
  const pathWithQuery = (() => {
    if (Array.from(param.entries()).length === 0) {
      return r.path;
    }
    return r.path + "?" + param.toString();
  })();

  const requestOption = {
     host,
      path: pathWithQuery,
       method,
        headers
      };
  console.warn(requestOption);

  const request = https.request(requestOption, response => {
    const chunks: string[] = [];

    console.warn('RESPONSE STATUS: ' + response.statusCode);
    console.warn('RESPONSE HEADERS: ' + JSON.stringify(response.headers));
    response.setEncoding('utf8');
    response.on('data', (chunk) => {
      if (chunk instanceof Buffer) {
        chunk = chunk.toString();
      }
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

  if (r.content !== undefined) {
    request.write(r.content.body);
    request.end();
  }
});

const sendGetRequest = (r: APIRequest) => {
  return sendRequest(r, "GET");
};

const sendPostRequest = (r: APIRequest) => {
  return sendRequest(r, "POST");
};

export const fetchAllGists = async ({ githubAuth }: { githubAuth: GitHubAuth }) => {
  const fetchSinglePageOfGistList = async (pageIndex: number) => {
    const userName = githubAuth.userName;
    // const since = "2018-01-01T00:00:00Z";
    const result = await sendGetRequest({
      path: `/users/${userName}/gists`,
      query:[ ["page", pageIndex.toString()]],
      content: {
        type: "application/json",
        body: "{}",
      },
      githubAuth,
    });
    const repos = JSON.parse(result);
    const summaries = repos.map((repo: GistMeta) => ({
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

const createGist = async ({ githubAuth }: { githubAuth: GitHubAuth }) => {
  const result = await sendPostRequest({
    path: "/gists",
    query: [],
    content: {
      type: "application/json",
      body: JSON.stringify({
        files: {
          "file1.txt": {
            content: "Hello, world!",
          },
        },
        description: "Automatically created via API."
      })
    },
    githubAuth
  });
  console.log("completed");
  console.log(result);
};
