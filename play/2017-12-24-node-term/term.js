const os = require("os");
const childProcess = require("child_process");
const fs = require("fs");
const path = require("path");

class TermView {
  constructor(termElement) {
    this.termElement = termElement;
    console.log(termElement);

    this.workingDirectoryElement =
      this.termElement.getElementsByClassName("term-working-directory")[0];
    console.log(this.workingDirectoryElement);
    this.termForm =
      this.termElement.getElementsByClassName("term-form")[0];
    this.outputTextbox =
      this.termElement.getElementsByClassName("term-output-textbox")[0];
    this.commandTextbox =
      this.termElement.getElementsByClassName("term-command-textbox")[0];
  }

  get workingDirectory() {
    return path.resolve(".");
  }

  // View methods.

  resetCommandTextbox() {
    const t = this.commandTextbox;

    t.value = "$ ";
    t.selectionStart = t.value.length;
    t.focus();
  }

  updateWorkingDirectory() {
    this.workingDirectoryElement.textContent = this.workingDirectory;
  }

  appendOutput(messages) {
    if (messages.length === 0) return;

    const t = this.outputTextbox;
    console.debug({ t, scrollTop: t.scrollTop, scrollHeight: t.scrollHeight, selectionStart: t.selectionStart, valueLength: t.value.length });
    // TODO: Improve.
    const autoScroll = t.selectionStart === t.value.length;

    t.value = [
      t.value,
      ...messages.map(line => line.trim() + os.EOL)
    ].join("");

    if (autoScroll) {
      t.scrollTop = t.scrollHeight;
      t.selectionStart = t.value.length;
    }
  }

  async submit() {
    const parseCommand = source => {
      source = source.replace(/\s/, " ").trim();

      if (source.indexOf("$") === 0) {
        source = source.substring(1).trim();
      }

      // TODO: validate or something?
      return source;
    };

    const commandSource = this.commandTextbox.value;

    const command = parseCommand(commandSource);
    if (command === undefined) {
      return;
    }

    return await this.execute(command);
  }

  async execute(command) {
    // TODO: exec を毎回実行するとワーキングディレクトリーが移動しない。
    return new Promise((resolve, reject) => {
      childProcess.exec(command, (error, stdout, stderr) => {
        const header = [
          "$ cd " + this.workingDirectory,
          "$ " + command,
        ];

        if (error) {
          console.error({ command, error, stdout, stderr });
          resolve({
            output: [
              ...header,
              stdout,
              `ERROR! #{error.name}`,
              "STDERR:",
              stderr,
            ],
          });
        } else {
          resolve({
            output: [
              ...header,
              stdout,
            ],
          });
        }
      });
    });
  }

  activate() {
    this.termForm.addEventListener("submit", async ev => {
      ev.preventDefault();
      const result = await this.submit();
      this.updateWorkingDirectory();
      this.appendOutput(result.output);
      this.resetCommandTextbox();
    });

    this.updateWorkingDirectory();
    this.resetCommandTextbox();
  }
}

const termElements = document.getElementsByClassName("term");

for (const termElement of termElements) {
  const termView = new TermView(termElement);
  termView.activate();
}
