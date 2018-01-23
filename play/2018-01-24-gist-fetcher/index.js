const dotenv = require("dotenv");
const passport = require("passport");
const OAuth2Strategy = require("passport-oauth2");
const { URL, URLSearchParams } = require("url");
const https = require("https");

dotenv.config();

const userInfo = {
  name: process.env.GITHUB_USER_NAME,
  password: process.env.GITHUB_USER_PASSWORD,
};

const accessToken = {
  clientID: process.env.GITHUB_APP_CLIENT_ID,
  clientSecret: process.env.GITHUB_APP_CLIENT_SECRET,
};

const sendRequest = (path, method, query, contentType, content) => new Promise((resolve, reject) => {
  const authorization = "Basic " + new Buffer(userInfo.name + ':' + userInfo.password).toString('base64');

  const request = https.request({
    host: "api.github.com",
    path,
    method,
    headers: {
      "Content-Type": contentType,
      "Content-Length": content.length,
      "User-Agent": "my-gist-fetcher",
      "Authorization": authorization,
    },
  }, response => {
    const chunks = [];

    console.warn('STATUS: ' + response.statusCode);
    console.warn('HEADERS: ' + JSON.stringify(response.headers));
    response.setEncoding('utf8');
    response.on('data', (chunk) => {
      console.warn('BODY: ' + chunk);
      chunks.push(chunk);
    });
    response.on("error", error => {
      reject(error);
    });
    response.on("end", () => {
      resolve(chunks);
    });
  });

  request.on("error", error => {
    reject(error);
  });

  request.write(content);
  request.end();
});

const sendGetRequest = (path, query, contentType, content) => {
  return sendRequest(path, "GET", query, contentType, content);
};

const sendPostRequest = (path, query, contentType, content) => {
  return sendRequest(path, "POST", query, contentType, content);
};

(async () => {
  const result = await sendGetRequest("/user", {}, "application/json", "");
  console.log("completed");
  console.log(result);
})();
