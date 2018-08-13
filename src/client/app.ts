import { app, h } from 'hyperapp';
import { Repl } from '../core/repl';

const repl = Repl.create();

interface Output {
  content: string;
}

interface AppState {
  source: string;
  outputs: Output[];
}

const initState: AppState = {
  source: `io {
  let _a = jslog("hello, world!")!
  2 * 3 * 4
}`,
  outputs: [],
};

const actions = {
  run: () => (state: AppState): AppState => {
    const source = (state.source || '').trim();
    if (source === '') {
      return state;
    }

    const result = repl.submit(source);
    state = actions.addLog({ content: JSON.stringify(result, undefined, 2) })(state);
    return { ...state, source: '' };
  },
  setSource: (source: string) => (state: AppState) => {
    return { ...state, source };
  },
  addLog: (output: Output) => (state: AppState): AppState => {
    return { ...state, outputs: [...state.outputs, output] };
  },
  onKeyPress: (ev: KeyboardEvent) => (state: AppState): AppState => {
    if (ev instanceof KeyboardEvent
      && ev.ctrlKey && !ev.shiftKey && !ev.metaKey
      && ev.key === 'Enter'
    ) {
      return actions.run()(state);
    }
    return state;
  },
};

const view = (state: AppState, actions: any) => {
  return h('section', { class: 'repl' }, [
    h('h3', {}, ['REPL:']),
    h('ol', { class: 'repl-logs' }, state.outputs.map(output => {
      return h('li', {}, [output.content]);
    })),
    h('section', { class: 'repl-controller' }, [
      h('textarea',
        {
          class: 'repl-editor',
          rows: 6,
          autofocus: true,
          value: state.source,
          oninput: (ev: any) =>
            actions.setSource(ev.target.value),
          onkeypress: (ev: any) =>
            actions.onKeyPress(ev),
        }, []),
      h('button',
        {
          class: 'repl-button repl-submit-button',
          type: 'button',
          onclick: () => actions.run(),
        }, [
          'Run',
        ]),
    ]),
  ]);
};

const appElem = document.getElementById('app');
const _ = app(initState, actions, view, appElem);
