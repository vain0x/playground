// NodeJS >= 8

const http = require("http");
const fs = require("fs");
const path = require("path");
const url = require("url");

const contentBase = "../dist";
const port = 8080;

const contentTypes = (() => {
  const mapping = {
    ".html": "text/html",
    ".css": "text/css",
    ".js": "application/javascript",
    ".mjs": "application/javascript",
    ".jpg": "image/jpeg",
    ".png": "image/png",
  };
  return ext => mapping[ext] || "text/plain";
})();

const server = http.createServer((request, response) => {
  try {
    console.debug(`Request ${request.url}`);

    const fullPath = (() => {
      const basePath = path.resolve(contentBase);
      const { pathname } = url.parse(request.url);
      const resolved = path.join(basePath, pathname);
      if (!resolved.startsWith(basePath)) {
        throw { code: 'ENOENT' };
      }
      if (resolved.endsWith("/")) {
        return path.join(resolved, "index.html");
      }
      return resolved;
    })();

    const buffer = fs.readFileSync(fullPath);
    const contentType = contentTypes(path.extname(fullPath));
    response.statusCode = 200;
    response.setHeader("Content-Type", contentType);
    response.write(buffer);
    return response.end();
  } catch (err) {
    if (err.code === 'ENOENT' || err.code === 'EISDIR') {
      response.statusCode = 404;
      console.log(`Not found ${request.url}`);
    } else {
      response.statusCode = 500;
      console.error(err);
    }
    return response.end();
  }
});

console.log(`Listening to http://localhost:${port} ...`);
server.listen(port);
