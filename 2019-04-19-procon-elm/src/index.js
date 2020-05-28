var Elm = require("./Main").Elm

var app = Elm.Main.init({
  flags: [],
})

process.stdin.on("data", function (buffer) {
  app.ports.readLine.send(buffer.toString())
})

app.ports.writeLine.subscribe(function (t) {
  var text = t[0]
  var end = t[1]

  process.stdout.write(text)
  process.stdout.write("\n")

  if (end) {
    process.stdout.end(function () {
      process.stdin.destroy()
    })
  }
});

app.ports.exit.subscribe(function () {
  process.stdin.destroy()
})
