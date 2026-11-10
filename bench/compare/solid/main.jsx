import { createSelector, createSignal, batch, For } from "solid-js";
import { render } from "solid-js/web";
import { buildData as buildPlain } from "../shared/data.js";

const buildData = (count) =>
  buildPlain(count).map(({ id, label }) => {
    const [get, set] = createSignal(label);
    return { id, label: get, setLabel: set };
  });

const Button = ({ id, text, fn }) => (
  <div class="col-sm-6 smallpad">
    <button id={id} class="btn btn-primary btn-block" type="button" onClick={fn}>{text}</button>
  </div>
);

const App = () => {
  const [data, setData] = createSignal([]);
  const [selected, setSelected] = createSignal(0);
  const isSelected = createSelector(selected);
  const run = () => setData(buildData(1000));
  const runLots = () => setData(buildData(10000));
  const add = () => setData((d) => [...d, ...buildData(1000)]);
  const update = () =>
    batch(() => {
      const d = data();
      for (let i = 0; i < d.length; i += 10) d[i].setLabel((l) => l + " !!!");
    });
  const swapRows = () => {
    const d = data().slice();
    if (d.length > 998) {
      const a = d[1];
      d[1] = d[998];
      d[998] = a;
      setData(d);
    }
  };
  const clear = () => setData([]);
  const remove = (id) => setData((d) => {
    const i = d.findIndex((r) => r.id === id);
    return [...d.slice(0, i), ...d.slice(i + 1)];
  });

  return (
    <div class="container">
      <div class="jumbotron">
        <div class="row">
          <div class="col-md-6"><h1>Solid keyed</h1></div>
          <div class="col-md-6">
            <div class="row">
              <Button id="run" text="Create 1,000 rows" fn={run} />
              <Button id="runlots" text="Create 10,000 rows" fn={runLots} />
              <Button id="add" text="Append 1,000 rows" fn={add} />
              <Button id="update" text="Update every 10th row" fn={update} />
              <Button id="clear" text="Clear" fn={clear} />
              <Button id="swaprows" text="Swap Rows" fn={swapRows} />
            </div>
          </div>
        </div>
      </div>
      <table class="table table-hover table-striped test-data">
        <tbody>
          <For each={data()}>
            {(row) => {
              const id = row.id;
              return (
                <tr class={isSelected(id) ? "danger" : ""}>
                  <td class="col-md-1" textContent={id} />
                  <td class="col-md-4">
                    <a class="lbl" onClick={[setSelected, id]} textContent={row.label()} />
                  </td>
                  <td class="col-md-1">
                    <a class="remove" onClick={[remove, id]}>
                      <span class="remove glyphicon glyphicon-remove" aria-hidden="true" />
                    </a>
                  </td>
                  <td class="col-md-6" />
                </tr>
              );
            }}
          </For>
        </tbody>
      </table>
      <span class="preloadicon glyphicon glyphicon-remove" aria-hidden="true" />
    </div>
  );
};

render(App, document.getElementById("main"));
