Vue.component('repl', {
  props: [],
  data: {
    outputs: [],
  },
  template: `
    <section class="repl">
      REPL:

      <section class="repl-controller">
        <textarea class="repl-editor" rows="3"></textarea>

        <button type="button" class="repl-button repl-submit-button">
          Run
        </button>
      </section>

      Output:

      <ol class="repl-logs">
        <li
          v-for="item in outputs"
          :key="item.key"
        >{{ item.content }}
        </li>
      </ol>
    </section>
    `,
});

const app = new Vue({
  el: '#app',
  template: `
    <repl id="main-content"></repl>
  `,
});
