// file-rename-many

// TODO: Support file deletion.
// TODO: Support directory renaming.

const os = require('os');
const fs = require('fs-extra');
const path = require('path');
const child_process = require('child_process');
const readlineSync = require('readline-sync');

// Simple transaction support.
const runTransaction = operations => {
    const undoList = [];
    let completed = false;
    try {
        for (const operation of operations) {
            const undo = operation();
            undoList.push(undo);
        }

        completed = true;
    } finally {
        if (!completed) {
            for (let i = undoList.length - 1; i >= 0; i--) {
                undoList[i]();
            }
        }
    }
};

// Terms:
//   File name: A relative path to a file or directory based on `root`.

const createModel = root => {
    const oldEntries =
        fs.readdirSync(root)
        .map(fileName => {
            const filePath = path.resolve(root, fileName);
            if (!fs.existsSync(filePath)) return null;
            const stat = fs.statSync(filePath);
            if (stat.isDirectory()) return null;
            return { fileName, filePath };
        })
        .filter(obj => obj !== null)
        .map((entry, fileId) => ({ fileId, ...entry }));

    const source =
        [
            "# Edit this script to rename files.",
            "# NOTE: Lines starting with # are ignored.",
            "",
            "# 👉 USAGE:",
            "# Each line with the format of:",
            "#    <number> <file-name>",
            "# represents a file. Edit file names to rename them.",
            "",
            "# ⚠　WARNING:",
            "# Don't change <number>s.",
        ].concat(
            oldEntries.map(entry => {
                const paddedId = entry.fileId.toString(10).padStart(8, '0');
                return `${paddedId} "${entry.fileName}"`;
            }))
        .concat([""])
        .join(os.EOL);

    const isCanceled = script => {
        return script.trim() === '';
    };

    const parse = script => {
        const regexp = new RegExp(String.raw`^0*(\d+) *(.*)$`);

        const used =
            oldEntries.map(entry => false);

        const lines =
            script.split(/\r\n|\r|\n/);

        const results =
            lines.map((line, lineIndex) => {
                const trimed = line.trim();
                if (trimed === "" || trimed.startsWith("#")) {
                    return { type: "ignore" };
                }

                const m = trimed.match(regexp);
                if (m === null || m.length !== 3) {
                    return {
                        type: "error",
                        message: `Invalid line at ${lineIndex}: ${line}`
                    };
                }

                const fileId = m[1] === "" ? 0 : parseInt(m[1], 10);
                const newName = m[2].replace(/"|'/g, "");

                if (!(0 <= fileId && fileId < oldEntries.length)) {
                    return { type: "error", message: `Invalid file Id at ${lineIndex}.` };
                }

                if (used[fileId]) {
                    return { type: "error", message: `Duplicated file Id at ${lineIndex}.` };
                }
                used[fileId] = true;

                const entry = oldEntries[fileId];

                // TODO: Validate file name.

                const newPath = path.resolve(root, newName);

                if (entry.filePath === newPath) {
                    return { type: "ignore" };
                }

                return {
                    type: "change",
                    lineIndex,
                    entry,
                    newName,
                    newPath,
                };
            })
            .filter(result => result.type !== "ignore");

        const errors =
            results
            .filter(result => result.type === "error")
            .map(result => result.message);

        if (errors.length > 0) {
            return { type: "error", messages: errors };
        }

        const changes = results.filter(result => result.type === "change");
        return { type: "change", changes };
    };

    const display = parseResult => {
        if (parseResult.type === "error") {
            const errors = parseResult.errors;
            console.log(`Error (${errors.length}):`);
            for (const error of errors) {
                console.log(`  ${error}`);
            }
            return;
        }

        if (parseResult.type !== "change") throw new Error();

        const changes = parseResult.changes;
        console.log(`${changes.length} files are going to be renamed:`);
        for (let change of changes) {
            console.log(`  ${change.entry.fileName}`);
            console.log(`    ==> ${change.newName}`);
        }
    }

    const compile = parseResult => {
        if (parseResult.type === "error") {
            return [];
        }

        if (parseResult.type !== "change") throw new Error();

        const instructions = [];

        for (const change of parseResult.changes) {
            instructions.push(() => {
                const dir = path.dirname(change.newPath);

                // TODO: Remove created directories during undoing.
                fs.mkdirs(dir);

                fs.renameSync(change.entry.filePath, change.newPath);
                return () => {
                    fs.renameSync(change.newPath, change.entry.filePath);
                };
            });
        }

        return instructions;
    };

    return { source, isCanceled, parse, display, compile };
};

// TODO: Fix.
const editorPath = os.type() === "Windows_NT" ? "notepad" : "nano";

const disposables = [];
const defer = dispose => disposables.push(dispose);
try {
    const root = path.join(process.cwd(), "/node_modules");

    const tempFilePath = path.join(os.tmpdir(), "file-rename-many-edit-script.txt");

    const model = createModel(root);

    fs.writeFileSync(tempFilePath, model.source);
    defer(() => {
        fs.unlinkSync(tempFilePath);
    });

    child_process.execSync(`${editorPath} '${tempFilePath}'`);

    const editedScript = fs.readFileSync(tempFilePath).toString();
    if (model.isCanceled(editedScript)) {
        return;
    }

    const parseResult = model.parse(editedScript);

    model.display(parseResult);

    if (!readlineSync.keyInYN("OK?")) {
        console.log("Canceled.");
        return;
    }

    const compilationResult = model.compile(parseResult);
    runTransaction(compilationResult);

} finally {
    for (const d of disposables) {
        d();
    }
}
