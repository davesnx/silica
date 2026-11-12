type classes = { dialog : Dialog.classes; combobox : Combobox.classes }

let no_classes = { dialog = Dialog.no_classes; combobox = Combobox.no_classes }

type props = {
  id : string;
  title : string;
  placeholder : string;
  empty : string;
  items : Combobox.item list;
  open_ : bool option;
  classes : classes;
}

let dialog =
  Dialog.component ~id:"command-menu"
    ~closes:(fun (_ : string) -> true)
    ~event_name:(fun _ -> "run")
    (fun props -> Combobox.make ~on_value_change:Fun.id props)

let props ~id ~title ?(placeholder = "") ?(empty = "No results") ?open_
    ?(classes = no_classes) items =
  { id; title; placeholder; empty; items; open_; classes }

let menu_props props =
  Dialog.props ~id:props.id ~title:props.title ?open_:props.open_
    ~classes:props.classes.dialog
    (Combobox.props ~id:(props.id ^ "-search") ~label:props.title
       ~placeholder:props.placeholder ~empty:props.empty ~inline:true
       ~classes:props.classes.combobox props.items
    )

let make ?key ?on_open_change ?on_run props =
  Dialog.make ?key ?on_open_change ?on_content:on_run dialog (menu_props props)
