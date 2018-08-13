import { Repl } from '../core/repl';

const repl = Repl.create();

const elems = {
  editor: document.getElementsByClassName('repl-editor')[0] as HTMLTextAreaElement,
  submitButton: document.getElementsByClassName('repl-submit-button')[0] as HTMLButtonElement,
  logs: document.getElementsByClassName('repl-logs')[0] as HTMLOListElement,
};

const actions = {
  addLog(value: any) {
    const fullText = JSON.stringify(value, undefined, 2);
    const isLong = fullText.length >= 80;
    const summaryText = !isLong ? fullText : fullText.substring(0, 20) + '...';

    const itemElem = document.createElement('li');
    if (isLong) {
      const detailsElem = document.createElement('details');
      detailsElem.textContent = fullText;
      const summaryElem = document.createElement('summary');
      summaryElem.textContent = summaryText;
      detailsElem.appendChild(summaryElem);
      itemElem.appendChild(detailsElem);
    } else {
      itemElem.textContent = fullText;
    }
    elems.logs.insertBefore(itemElem, elems.logs.firstChild);
  },

  submit() {
    const source = (elems.editor.value || '').trim();
    if (source === '') {
      return;
    }

    const result = repl.submit(source);
    actions.addLog(result);

    elems.editor.value = '';
  },
};

elems.editor.addEventListener('keypress', ev => {
  if (!(ev instanceof KeyboardEvent)) { return; }

  if (ev.ctrlKey && !ev.shiftKey && !ev.metaKey && ev.key === 'Enter') {
    actions.submit();
  }
});

elems.submitButton.addEventListener('click', _ => {
  actions.submit();
});

// tslint:disable
console.debug("ready");
console.debug(elems);
console.debug(actions);
