// Merge all branches into different directories of single branch by using git-subtree-add.
// Uses many adhoc ways for the VainZero.Sandbox.CSharp repo.

const childProcess = require("child_process");
const getStdin = require("get-stdin");

const takeUntil = (str, delimiter) => {
  const i = str.indexOf(delimiter);
  return i >= 0 ? str.substr(0, i) : null;
};

const last = array => {
  return array.length >= 1 ? array[array.length - 1] : null;
};

const lastLine = note => {
  return last(note.split("\n").filter(line => line !== ""));
};

const exec = async command => {
  return new Promise((resolve, reject) => {
    childProcess.exec(command, (error, stdout, stderr) => {
      if (error) {
        reject(error);
      } else {
        resolve(stdout);
      }
    });
  });
};

const main = async () => {
  const input = await getStdin();

  const regexp = new RegExp("origin/([\\w\\d-_.]+)");
  const blacklist = ["HEAD", "master", "lib", "cli", "wpf", "pg"];

  for (const line of input.split("\n")) {
    const r = line.match(regexp);
    if (r === null) continue;

    const branchName = r[1];
    if (blacklist.includes(branchName)) continue;

    const dotIndex = branchName.indexOf(".");
    const baseBranchName = takeUntil(branchName, ".") ||"master";
    const rawBranchName = branchName.substr(dotIndex >= 0 ? dotIndex + 1 : 0);

    const findFirstCommitDate = async baseCommit => {
      const range = `${baseCommit}..origin/${branchName}`;

      const subjects = await exec(`git log ${range} --pretty=format:'%s'`);
      const subject = lastLine(subjects);
      if (subject === "Empty"
        || subject === "Add project"
        || subject === "Add VainZero.SandBox.Wpf project"
      ) {
        return null;
      }

      const dates = await exec(`git log ${range} --pretty=format:'%ad' --date=format:'%Y-%m-%d'`);
      const date = lastLine(dates);

      return { baseCommit, subject, date, range };
    };

    let orig = null;
    for (const baseCommit of ["origin/" + baseBranchName, "970feb2", "deb6e08", "e451b3f", "69c6a6e"]) {
      const d = await findFirstCommitDate(baseCommit);
      if (d === null) continue;
      orig = d;
      break;
    }

    if (orig === null) {
      console.log("Unknown origin date: " + branchName);
      continue;
    }

    console.log("");
    console.log("> " + orig.range);

    const command = [
      `git subtree add -P play/${orig.date}-VainZero.Sandbox.CSharp-${branchName.replace(".", "-")} . ${branchName}`
    ].join(" && ");
    console.log(command);
    await exec(command);
  }
};

main();
