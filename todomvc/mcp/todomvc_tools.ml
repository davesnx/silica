open Silica
open Todomvc_todos

let id =
  Mcp.Input.(
    field "id"
      ~description:
        "The number of the todo: read shows it in the name of its checkbox, \
         such as checkbox \"Todo 1: Buy milk\"."
      int
  )

let annotations ~destructive ~idempotent =
  Mcp.annotations ~read_only:false ~destructive ~idempotent ~open_world:false ()

let draft =
  Mcp.make ~name:"todo.draft" ~title:"Type a new todo"
    ~description:
      "Set the whole text of the field \"What needs to be done?\". Nothing is \
       added until todo.add."
    ~annotations:(annotations ~destructive:false ~idempotent:true)
    Mcp.Input.(field "text" ~description:"The whole text of the field." string)

let add =
  Mcp.make ~name:"todo.add" ~title:"Add a todo"
    ~description:
      "Add a todo with this text, trimmed, at the end of the list, as Enter in \
       the field \"What needs to be done?\" does. Empty text is refused."
    ~annotations:(annotations ~destructive:false ~idempotent:false)
    Mcp.Input.(field "text" ~description:"The text of the todo." string)

let toggle =
  Mcp.make ~name:"todo.toggle" ~title:"Toggle a todo"
    ~description:
      "Mark the todo completed, or active again when it is completed."
    ~annotations:(annotations ~destructive:true ~idempotent:false)
    id

let toggle_all =
  Mcp.make ~name:"todo.toggle_all" ~title:"Toggle all todos"
    ~description:
      "Mark every todo completed, or every todo active when all are completed."
    ~annotations:(annotations ~destructive:true ~idempotent:false)
    Mcp.Input.none

let destroy =
  Mcp.make ~name:"todo.destroy" ~title:"Delete a todo"
    ~description:"Delete the todo."
    ~annotations:(annotations ~destructive:true ~idempotent:true)
    id

let clear_completed =
  Mcp.make ~name:"todo.clear_completed" ~title:"Clear completed todos"
    ~description:"Delete every completed todo."
    ~annotations:(annotations ~destructive:true ~idempotent:true)
    Mcp.Input.none

let edit =
  Mcp.make ~name:"todo.edit" ~title:"Edit a todo"
    ~description:
      "Open the todo for editing, as a double click on its text does. While a \
       todo is open, another tool of a todo saves the open one first."
    ~annotations:(annotations ~destructive:false ~idempotent:true)
    id

let edit_draft =
  Mcp.make ~name:"todo.edit_draft" ~title:"Type into the open todo"
    ~description:
      "Set the whole text of the open todo. Nothing is saved until todo.save."
    ~annotations:(annotations ~destructive:false ~idempotent:true)
    Mcp.Input.(field "text" ~description:"The whole text of the todo." string)

let save =
  Mcp.make ~name:"todo.save" ~title:"Save the open todo"
    ~description:
      "Save the text of the open todo, trimmed, as Enter does. Empty text \
       deletes the todo."
    ~annotations:(annotations ~destructive:true ~idempotent:true)
    Mcp.Input.none

let cancel =
  Mcp.make ~name:"todo.cancel" ~title:"Cancel the edit"
    ~description:"Close the open todo and keep its old text, as Escape does."
    ~annotations:(annotations ~destructive:false ~idempotent:true)
    Mcp.Input.none

let todos =
  Mcp.tools machine
    [
      Mcp.on draft (fun text -> Draft text);
      Mcp.on add (fun text -> Add text);
      Mcp.on toggle (fun id -> Toggle id);
      Mcp.on toggle_all (fun () -> Toggle_all);
      Mcp.on destroy (fun id -> Destroy id);
      Mcp.on clear_completed (fun () -> Clear_completed);
      Mcp.on edit (fun id -> Edit id);
      Mcp.on edit_draft (fun text -> Edit_draft text);
      Mcp.on save (fun () -> Save_edit);
      Mcp.on cancel (fun () -> Cancel_edit);
    ]

let publications = [ todos ]
