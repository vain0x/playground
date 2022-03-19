import Head from "next/head"
import { Dispatch, memo, SetStateAction, useEffect, useRef, useState } from "react"

export default function Home() {
  const [entries, setEntries] = useState(Entries.EMPTY)

  return (
    <>
      <Head>
        <title>Example: React with Classes</title>
      </Head>

      <main id="daily-page" style={{ margin: "1rem" }}>
        <table style={{ tableLayout: "fixed", border: "2px solid #ddd" }}>
          <tr style={{ height: "3.5rem", background: "#f3f3f3" }}>
            <th style={{ padding: "0 0.5rem", width: "3.5rem", textAlign: "right" }}>時刻</th>
            <th style={{ width: "8rem" }}>タスク</th>
            <th style={{ width: "4rem" }} aria-label="操作" />
          </tr>

          {N24.map((hour: Hour) => (
            <EntryRow key={hour} hour={hour} entry={entries.at(hour)} setEntries={setEntries} />
          ))}
        </table>
      </main>
    </>
  )
}

const EntryRow = memo(function EntryRow(props: {
  hour: number
  entry?: Entry
  setEntries: Dispatch<SetStateAction<Entries>>
}) {
  const { hour, entry, setEntries } = props
  const ref = useRef<HTMLTableRowElement>(null)

  // 更新を強調表示する。
  useEffect(() => {
    const e = ref.current
    if (e != null) {
      e.style.outline = "2px solid #aec"
      const h = setTimeout(() => { e.style.outline = "" }, 500)
      return () => { clearTimeout(h) }
    }
  })

  return (
    <tr ref={ref}>
      <td style={{ padding: "0 0.5rem", textAlign: "right" }}>{hour}時</td>

      <td style={{ height: "2rem" }}>
        {entry != null ? (
          <select
            style={{ margin: "0 0.5rem" }}
            onChange={ev => {
              const value = +ev.target.value
              const taskId = Number.isInteger(value) ? value : undefined
              setEntries(entries => entries.updateAt(hour, e => e.withTask(taskId)))
            }}>
            <option hidden></option>
            {TASKS.map((name, i) => (
              <option value={i + 1}>{name}</option>
            ))}
          </select>
        ) : (
          <button type="button" className="add-button" onClick={() => {
            setEntries(entries => entries.push(new Entry(hour)))
          }} aria-label="追加する" />
        )}
      </td>

      <td>
        {entry != null ? (
          <button type="button"
            onClick={() => {
              setEntries(entries => entries.remove(hour))
            }}
            style={{ border: "none", color: "#e44" }}>
            削除
          </button>
        ) : null}
      </td>
    </tr>
  )
})

// -----------------------------------------------
// モデル
// -----------------------------------------------

class Entries {
  readonly #items: Entry[]

  private constructor(items: Entry[]) {
    this.#items = items
  }

  public static EMPTY = new Entries([])

  public get length(): number {
    return this.#items.length
  }

  public at(hour: Hour): Entry | undefined {
    return this.#items.find(e => e.startHour === hour)
  }

  public updateAt(hour: Hour, change: (e: Entry) => Entry): Entries {
    const e = this.at(hour)
    if (e == null) return this

    const newEntry = change(e)
    if (newEntry === e) return this

    return this.remove(hour).push(newEntry)
  }

  public push(entry: Entry): Entries {
    return new Entries([...this.#items, entry])
  }

  public remove(startHour: Hour): Entries {
    const index = this.#items.findIndex(e => e.startHour === startHour)
    return index >= 0
      ? new Entries(this.#items.filter((_, i) => i !== index))
      : this
  }
}

class Entry {
  readonly #start: Hour
  readonly #taskId: TaskId | undefined

  public constructor(start: Hour, taskId?: TaskId) {
    this.#start = start
    this.#taskId = taskId
  }

  public get startHour(): number { return this.#start }

  public withTask(taskId: TaskId | undefined): Entry {
    return this.#taskId !== taskId
      ? new Entry(this.#start, taskId)
      : this
  }
}

type Hour = number
type TaskId = number

const TASKS = [
  "朝食",
  "昼食",
  "夕食",
  "風呂",
  "睡眠",
  "掃除",
  "洗濯",
]

// ===============================================

const range = (start: number, end: number): number[] => {
  const output: number[] = []
  for (let i = start; i < end; i++) output.push(i)
  return output
}

const N24 = range(0, 24)
