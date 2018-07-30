use std::collections::HashSet;
use std::io::{stderr, Write};
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use std::str;

// ref:
// https://git-scm.com/docs/git-rev-parse
// https://git-scm.com/docs/git-merge-base

macro_rules! args {
    ($($a:expr),*) => {{
        let mut v = Vec::<String>::new();
        $(v.push($a.to_string());)*
        v
    }};
}

fn read(data: &[u8]) -> &str {
    str::from_utf8(data).unwrap()
}

type CommitHash = String;

type Result<T> = std::result::Result<T, std::io::Error>;

fn make_error<T>(message: String) -> Result<T> {
    Err(std::io::Error::new(std::io::ErrorKind::Other, message))
}

struct GitCarve {
    work_dir: Option<PathBuf>,
    trunk: String,
    carve: String,
    ignore_heads: HashSet<String>,
    dry: bool,
}

impl GitCarve {
    fn new(work_dir: Option<PathBuf>) -> GitCarve {
        let trunk = "develop".to_owned();
        let carve = "__carve".to_owned();
        let ignore_heads = vec!["master".to_owned(), trunk.clone(), carve.clone()]
            .into_iter()
            .collect();

        GitCarve {
            work_dir,
            trunk,
            carve,
            ignore_heads,
            dry: false,
        }
    }

    fn set_dry(&mut self, dry: bool) -> &mut GitCarve {
        self.dry = dry;
        self
    }

    fn git_command(&self, args: Vec<String>) -> Command {
        let mut command = Command::new("git");
        command.args(args);
        command.stderr(Stdio::piped());
        if let Some(work_dir) = &self.work_dir {
            command.current_dir(work_dir);
        }
        command
    }

    fn git_output(&self, mut command: Command) -> Result<Output> {
        eprintln!("$ {:?}", command);
        command.output()
    }

    fn git_determine(&self, args: Vec<String>) -> bool {
        let command = self.git_command(args);
        match self.git_output(command) {
            Err(_) => false,
            Ok(output) => output.status.success(),
        }
    }

    fn git_lines(&self, args: Vec<String>) -> Result<Vec<String>> {
        let command = self.git_command(args);
        let output = self.git_output(command)?;

        if !output.status.success() {
            stderr().write_all(&output.stderr).unwrap_or_default();
            return make_error(format!("Non-zero exit code {:?}", output.status.code()));
        }

        let lines = read(&output.stdout)
            .lines()
            .map(|line| line.to_owned())
            .collect();
        Ok(lines)
    }

    fn git_do(&self, args: Vec<String>) -> Result<()> {
        let command = self.git_command(args);

        if self.dry {
            eprintln!("$ {:?}", command);
            return Ok(());
        }

        let output = self.git_output(command)?;

        if !output.status.success() {
            stderr().write_all(&output.stderr).unwrap_or_default();
            return make_error(format!("Non-zero exit code {:?}", output.status.code()));
        }

        Ok(())
    }

    fn get_commit_date(&self, c: &CommitHash) -> Result<String> {
        let lines = self.git_lines(args!["show", "-s", "--format=%ci", c])?;
        match lines.into_iter().next() {
            Some(line) => Ok(line),
            None => make_error(format!("No commit date of {}", c)),
        }
    }

    fn is_ancestor(&self, ca: &CommitHash, cd: &CommitHash) -> bool {
        self.git_determine(args!["merge-base", "--is-ancestor", ca, cd])
    }

    // The most recent commit that the trunk/tip branches share.
    fn fork_point(&self, tip: &CommitHash) -> Result<Option<String>> {
        let fork: CommitHash = match self
            .git_lines(args!["merge-base", &self.trunk, tip])?
            .into_iter()
            .next()
        {
            Some(x) => x,
            None => return make_error(format!("No fork point {}", tip)),
        };

        Ok(Some(fork))
    }

    fn ignore(&self, commit: &CommitHash) -> bool {
        self.ignore_heads
            .iter()
            .any(|ignored| self.is_ancestor(commit, ignored))
    }

    fn branches(&self) -> Result<Vec<String>> {
        Ok(self
            .git_lines(args!["rev-parse", "--symbolic", "--branches"])?
            .into_iter()
            .filter(|commit| !self.ignore(commit))
            .collect())
    }

    fn branch_out(&self, branch: &str, commit: &str) -> Result<()> {
        self.git_do(args!["checkout", "-b", branch, commit])
    }

    fn checkout(&self, branch: &str) -> Result<()> {
        self.git_do(args!["checkout", branch])
    }

    fn revert_all(&self, fork: &str, tip: &String) -> Result<bool> {
        // Commits to be reverted.
        let commits = self.git_lines(args![
            "rev-list",
            format!("{}..{}", fork, tip),
            "--no-merges",
            "--topo-order"
        ])?;

        if commits.is_empty() {
            return Ok(false);
        }

        // Ignore this tip if merged into carve branch by previous iterations.
        if self.is_ancestor(tip, &self.carve) {
            return Ok(false);
        }

        for commit in commits {
            self.git_do(args!["revert", "--no-edit", commit])?;
        }

        Ok(true)
    }

    fn merge_auto(&self, c: &str, message: String) -> Result<()> {
        self.git_do(args!["merge", c, "--no-ff", "-m", message])
    }

    fn discard_all(&self) -> Result<()> {
        self.git_do(args!["reset", "--hard"])
    }

    fn run(&self) -> Result<()> {
        let branches = self.branches()?;

        let forks = {
            let mut forks = vec![];
            for br in branches {
                let fork = match self.fork_point(&br)? {
                    Some(fork) => fork,
                    None => continue,
                };
                let date = self.get_commit_date(&fork)?;
                forks.push((br, fork, date));
            }
            forks.sort_by(|(_, _, l), (_, _, r)| l.cmp(r));
            forks
        };

        let tips = forks
            .iter()
            .map(|(tip, _, _)| tip.to_owned())
            .collect::<Vec<String>>();

        self.branch_out(&self.carve, &self.trunk)?;

        for (tip, fork, _) in forks {
            let wip = "__wip";
            let sprout = "__sprout";

            // On carve branch.

            // Revert all commits.
            self.branch_out(wip, &tip)?;

            if !self.revert_all(&fork, &tip)? {
                self.checkout(&self.carve)?;
                self.git_do(args!["branch", "-d", wip])?;
                continue;
            }

            // Merge into trunk avoiding conflict.
            self.branch_out(sprout, &fork)?;
            self.merge_auto(wip, format!("Merge: Revert all"))?;

            // Merge into feature branch.
            let merge_sprout = || {
                self.checkout(&self.carve)?;
                self.merge_auto(sprout, format!("Merge: Carve branch '{}'", tip))
            };

            match merge_sprout() {
                Err(_) => {
                    // If conflict, merge trunk into carve and retry.
                    self.discard_all()?;
                    self.checkout(sprout)?;
                    self.merge_auto(&self.trunk, format!("Merge branch '{}'", &self.trunk))?;

                    merge_sprout()?;
                }
                Ok(()) => {}
            }

            // Remove temporary branches.
            self.git_do(args!["branch", "-d", wip, sprout])?;
        }

        // Remove branches.
        self.checkout(&self.carve)?;
        self.git_do(
            args!["branch", "-d"]
                .into_iter()
                .chain(tips.into_iter())
                .collect(),
        )?;

        // Merge working branch into trunk.
        self.checkout(&self.trunk)?;
        self.merge_auto(&self.carve, format!("Merge: Carve branches"))?;
        self.git_do(args!["branch", "-d", &self.carve])?;

        Ok(())
    }
}

fn is_dir(d: &PathBuf) -> bool {
    match std::fs::metadata(d) {
        Ok(ref meta) if meta.is_dir() => true,
        Err(_) => {
            eprintln!("No metadata for {}", d.display());
            false
        }
        Ok(_) => {
            eprintln!("Not found {}", d.display());
            false
        }
    }
}

fn main() {
    if std::env::args().any(|t| t == "-h" || t == "--help") {
        eprintln!(
            r#"USAGE: GIT_CARVE_WORK_DIR=. GIT_CARVE_DRY=false git-carve

Description
-----------

Revert all non-merged commits into `develop` branch
so that they are recorded in the history.

Env vars
--------

GIT_CARVE_DIR
    Path to Git repository dir. Defaults to pwd.

GIT_CARVE_DRY
    Dry run? Defaults to `true`.
"#
        );
        std::process::exit(1)
    }

    let work_dir = std::env::var("GIT_CARVE_WORK_DIR")
        .ok()
        .map(|dir| PathBuf::from(dir))
        .filter(is_dir);

    let dry = std::env::var("GIT_CARVE_DRY")
        .ok()
        .map(|value| value.parse::<bool>().unwrap())
        .unwrap_or(true);

    GitCarve::new(work_dir).set_dry(dry).run().unwrap()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_boolean() {
        assert_eq!("true".parse::<bool>().unwrap(), true);
        assert_eq!("false".parse::<bool>().unwrap(), false);
    }
}
