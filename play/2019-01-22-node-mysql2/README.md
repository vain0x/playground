Trial of 'node-mysql2' package in TypeScript.

## Usage

1. Install Docker
2. Build a docker image with `./docker-build`
3. Start container with `./docker-run`
    - MySQL server works in the container.
4. Play with the MySQL server with `yarn start`
    - The script `./src/index.ts` submits a query to the server to fetch some data and prints them.
    - You might need to wait for a while until the server gets ready.
5. Stop container with `docker stop node-mysql2`
