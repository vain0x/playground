// 定数: 収縮速度 V、近傍半径 W
// 近い点を2回タップするとノーツが生成される。
// 2回のタップの間の時間を t として、ノーツは半径 r=t/V を持つ。
// ノーツの半径は速度 V で減少する。
// 半径が W 以下のときにノーツ付近をタップすると、エフェクトを表示し、スコアを加算して、半径を t/V に戻す。
// タップされないまま W/V 時間が経過したノーツは消滅する。

const TAU = 2 * Math.PI
const EPS = 1e-6

const TICK_MILLIS = 16

const PERFECT_TICKS = 2
const GREAT_TICKS = 100 / TICK_MILLIS
const GOOD_TICKS = 300 / TICK_MILLIS
const MIN_RADIUS = 1
const MIN_TICKS = GOOD_TICKS

const NOTE_RADIUS = 16
const NOTE_INITIAL_RADIUS = 30

const SPROUT_TICKS = 2000 / TICK_MILLIS
const SPROUT_RADIUS = NOTE_RADIUS

const pointNew = (x, y) => ({ x, y })

const pointSub = (p, q) => pointNew(p.x - q.x, p.y - q.y)

const pointNorm2 = p => p.x * p.x + p.y * p.y

const pointsAreNear = (p, q, r) =>
  pointNorm2(pointSub(p, q)) < r * r + EPS

const gameInit = () => ({
  sprout: null,
  notes: [],
  score: 0,
  tick: 0,
})

const sproutIsActivated = (sprout, click) =>
  sprout != null
  && pointsAreNear(sprout.point, click.point, SPROUT_RADIUS)
  && click.tick < sprout.tick + SPROUT_TICKS
  && click.tick >= sprout.tick + MIN_TICKS

const goodnessFromDifference = difference => {
  if (difference <= PERFECT_TICKS) {
    return "PERFECT"
  }

  if (difference <= GREAT_TICKS) {
    return "GREAT"
  }

  if (difference <= GOOD_TICKS) {
    return "GOOD"
  }

  return "MISS"
}

const goodnessToScore = goodness => {
  switch (goodness) {
    case "PERFECT":
      return 2

    case "GREAT":
      return 1

    default:
      return 0
  }
}

const goodnessToColor = goodness => {
  switch (goodness) {
    case "PERFECT":
      return "blue"

    case "GREAT":
      return "pink"

    case "GOOD":
      return "yellow"

    case "MISS":
      return "grey"

    default:
      return "white"
  }
}

const noteNew = (first, second) => {
  const point = second.point
  const start = second.tick
  const radius = NOTE_INITIAL_RADIUS
  const ticks = second.tick - first.tick
  const vel = radius / ticks
  return {
    point,
    vel,
    radius,
    ticks,
    start,
    click: null,
    goodness: null,
    animationTicks: 0,
    isLost: false,
  }
}

const noteIsNew = (note, tick) =>
  Math.abs(note.start - tick) <= 1

const noteIsNear = (note, point) =>
  pointsAreNear(point, note.point, NOTE_RADIUS)

const noteToDifference = (note, tick) =>
  Math.abs(note.start + note.ticks - tick)

const noteLoss = note => {
  note.click = null
  note.animationTicks = GOOD_TICKS
  note.goodness = "MISS"
  note.isLost = true
}

const gameOnClick = (state, p) => {
  const click = { point: p, tick: state.tick }

  for (const note of state.notes) {
    if (!noteIsNear(note, click.point) || note.isLost) {
      continue
    }

    if (note.click == null && !noteIsNew(note, state.tick)) {
      note.click = click
      note.goodness = goodnessFromDifference(noteToDifference(note, click.tick))
      note.animationTicks = GOOD_TICKS

      if (note.goodness === "MISS") {
        noteLoss(note)
      } else if (note.start + note.ticks >= click.tick) {
        gameRefreshNote(state, note)
      }
    }
    return
  }

  if (sproutIsActivated(state.sprout, click)) {
    state.notes.push(noteNew(state.sprout, click))
    state.sprout = null
    return
  }

  state.sprout = click
}

const gameRefreshNote = (state, note) => {
  state.score += goodnessToScore(note.goodness)

  note.point = note.click.point
  note.radius = NOTE_INITIAL_RADIUS
  note.start = state.tick
  note.click = null
}

const gameOnTick = state => {
  state.tick++

  for (const note of state.notes) {
    note.radius -= note.vel

    if (note.animationTicks > 0) {
      note.animationTicks--
    }

    if (note.isLost) {
      continue
    }

    const difference = noteToDifference(note, state.tick)
    if (difference === 0 && note.click != null) {
      gameRefreshNote(state, note)
    } else if (note.start + note.ticks + GOOD_TICKS < state.tick) {
      noteLoss(note)
    }
  }

  state.notes = state.notes.filter(note => !note.isLost || note.animationTicks > 0)
}

const gameRender = (state, size, rx) => {
  rx.clearRect(0, 0, size.width, size.height)

  for (const { point: { x, y }, radius, goodness, animationTicks } of state.notes) {
    const color = animationTicks > 0 ? goodnessToColor(goodness) : "white"

    const r = Math.max(radius, MIN_RADIUS)
    rx.beginPath()
    rx.strokeStyle = color
    rx.fillStyle = color
    rx.ellipse(x, y, r, r, 0, 0, TAU)
    rx.stroke()
    rx.fill()
  }
}

const main = () => {
  const appScoreElement = document.getElementById("app-score")
  const appCanvasElement = document.getElementById("app-canvas")

  const state = gameInit()

  appCanvasElement.addEventListener("click", ev => {
    const rect = appCanvasElement.getBoundingClientRect()
    const point = {
      x: ev.clientX - rect.left,
      y: ev.clientY - rect.top,
    }

    gameOnClick(state, point)
  })

  setInterval(() => {
    gameOnTick(state)
  }, TICK_MILLIS)

  setInterval(() => {
    window.requestAnimationFrame(() => {
      const rx = appCanvasElement.getContext("2d")
      gameRender(state, appCanvasElement, rx)

      appScoreElement.textContent = `${state.score} 点`
    })
  }, 60 / 1000)
}

document.addEventListener("DOMContentLoaded", main)
