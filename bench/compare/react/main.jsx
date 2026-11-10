import { memo, useReducer } from "react";
import { createRoot } from "react-dom/client";
import { buildData } from "../shared/data.js";

const initial = { data: [], selected: 0 };

const reducer = (state, action) => {
  const { data, selected } = state;
  switch (action.type) {
    case "RUN":
      return { data: buildData(1000), selected: 0 };
    case "RUN_LOTS":
      return { data: buildData(10000), selected: 0 };
    case "ADD":
      return { data: data.concat(buildData(1000)), selected };
    case "UPDATE": {
      const next = data.slice();
      for (let i = 0; i < next.length; i += 10) {
        const r = next[i];
        next[i] = { id: r.id, label: r.label + " !!!" };
      }
      return { data: next, selected };
    }
    case "CLEAR":
      return { data: [], selected: 0 };
    case "SWAP_ROWS": {
      if (data.length <= 998) return state;
      const next = data.slice();
      const a = next[1];
      next[1] = next[998];
      next[998] = a;
      return { data: next, selected };
    }
    case "REMOVE": {
      const i = data.findIndex((r) => r.id === action.id);
      return { data: [...data.slice(0, i), ...data.slice(i + 1)], selected };
    }
    case "SELECT":
      return { data, selected: action.id };
  }
  return state;
};

const Row = memo(({ item, selected, dispatch }) => (
  <tr className={selected ? "danger" : ""}>
    <td className="col-md-1">{item.id}</td>
    <td className="col-md-4">
      <a className="lbl" onClick={() => dispatch({ type: "SELECT", id: item.id })}>{item.label}</a>
    </td>
    <td className="col-md-1">
      <a className="remove" onClick={() => dispatch({ type: "REMOVE", id: item.id })}>
        <span className="remove glyphicon glyphicon-remove" aria-hidden="true" />
      </a>
    </td>
    <td className="col-md-6" />
  </tr>
));

const Button = ({ id, title, onClick }) => (
  <div className="col-sm-6 smallpad">
    <button type="button" className="btn btn-primary btn-block" id={id} onClick={onClick}>{title}</button>
  </div>
);

const Main = () => {
  const [{ data, selected }, dispatch] = useReducer(reducer, initial);
  return (
    <div className="container">
      <div className="jumbotron">
        <div className="row">
          <div className="col-md-6"><h1>React keyed</h1></div>
          <div className="col-md-6">
            <div className="row">
              <Button id="run" title="Create 1,000 rows" onClick={() => dispatch({ type: "RUN" })} />
              <Button id="runlots" title="Create 10,000 rows" onClick={() => dispatch({ type: "RUN_LOTS" })} />
              <Button id="add" title="Append 1,000 rows" onClick={() => dispatch({ type: "ADD" })} />
              <Button id="update" title="Update every 10th row" onClick={() => dispatch({ type: "UPDATE" })} />
              <Button id="clear" title="Clear" onClick={() => dispatch({ type: "CLEAR" })} />
              <Button id="swaprows" title="Swap Rows" onClick={() => dispatch({ type: "SWAP_ROWS" })} />
            </div>
          </div>
        </div>
      </div>
      <table className="table table-hover table-striped test-data">
        <tbody>
          {data.map((item) => (
            <Row key={item.id} item={item} selected={selected === item.id} dispatch={dispatch} />
          ))}
        </tbody>
      </table>
      <span className="preloadicon glyphicon glyphicon-remove" aria-hidden="true" />
    </div>
  );
};

createRoot(document.getElementById("main")).render(<Main />);
