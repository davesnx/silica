import { buildData } from "../shared/data.js";

const buttons = [
  ["run", "Create 1,000 rows"],
  ["runlots", "Create 10,000 rows"],
  ["add", "Append 1,000 rows"],
  ["update", "Update every 10th row"],
  ["clear", "Clear"],
  ["swaprows", "Swap Rows"],
];

document.getElementById("main").innerHTML =
  '<div class="container"><div class="jumbotron"><div class="row"><div class="col-md-6"><h1>Vanilla keyed</h1></div>' +
  '<div class="col-md-6"><div class="row">' +
  buttons
    .map(([id, title]) => `<div class="col-sm-6 smallpad"><button type="button" class="btn btn-primary btn-block" id="${id}">${title}</button></div>`)
    .join("") +
  '</div></div></div></div><table class="table table-hover table-striped test-data"><tbody></tbody></table>' +
  '<span class="preloadicon glyphicon glyphicon-remove" aria-hidden="true"></span></div>';

const tbody = document.querySelector("tbody");
const template = document.createElement("template");
template.innerHTML =
  '<tr><td class="col-md-1"> </td><td class="col-md-4"><a class="lbl"> </a></td>' +
  '<td class="col-md-1"><a class="remove"><span class="remove glyphicon glyphicon-remove" aria-hidden="true"></span></a></td>' +
  '<td class="col-md-6"></td></tr>';
const rowTemplate = template.content.firstChild;

let data = [];
let rows = [];
let selectedRow = null;

const createRow = (item) => {
  const tr = rowTemplate.cloneNode(true);
  const idCell = tr.firstChild;
  const lbl = idCell.nextSibling.firstChild;
  idCell.firstChild.nodeValue = item.id;
  lbl.firstChild.nodeValue = item.label;
  tr.dataset.id = item.id;
  return tr;
};

const append = (items) => {
  const fragment = document.createDocumentFragment();
  for (const item of items) {
    const tr = createRow(item);
    rows.push(tr);
    fragment.appendChild(tr);
  }
  tbody.appendChild(fragment);
  data = data.concat(items);
};

const clear = () => {
  tbody.textContent = "";
  data = [];
  rows = [];
  selectedRow = null;
};

const actions = {
  run: () => {
    clear();
    append(buildData(1000));
  },
  runlots: () => {
    clear();
    append(buildData(10000));
  },
  add: () => append(buildData(1000)),
  update: () => {
    for (let i = 0; i < data.length; i += 10) {
      data[i] = { id: data[i].id, label: data[i].label + " !!!" };
      rows[i].childNodes[1].firstChild.firstChild.nodeValue = data[i].label;
    }
  },
  clear,
  swaprows: () => {
    if (data.length <= 998) return;
    const a = rows[1];
    const b = rows[998];
    const afterB = b.nextSibling;
    tbody.insertBefore(b, a);
    tbody.insertBefore(a, afterB);
    rows[1] = b;
    rows[998] = a;
    const d = data[1];
    data[1] = data[998];
    data[998] = d;
  },
};

document.querySelector(".jumbotron").addEventListener("click", (e) => {
  const action = actions[e.target.id];
  if (action) action();
});

tbody.addEventListener("click", (e) => {
  const a = e.target.closest("a");
  if (!a) return;
  const tr = a.closest("tr");
  if (a.classList.contains("lbl")) {
    if (selectedRow) selectedRow.className = "";
    tr.className = "danger";
    selectedRow = tr;
  } else {
    const i = rows.indexOf(tr);
    if (selectedRow === tr) selectedRow = null;
    tr.remove();
    rows.splice(i, 1);
    data.splice(i, 1);
  }
});
