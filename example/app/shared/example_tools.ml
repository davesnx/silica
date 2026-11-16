open Silica

let draft =
  Mcp.make ~name:"edit.draft" ~description:"Set the whole draft."
    ~annotations:(Mcp.annotations ~idempotent:true ())
    Mcp.Input.(field "text" ~description:"The whole draft." string)

let save =
  Mcp.make ~name:"edit.save" ~description:"Save the draft."
    ~annotations:(Mcp.annotations ~idempotent:true ())
    Mcp.Input.none

let leave =
  Mcp.make ~name:"edit.leave" ~description:"Leave the page and drop the draft."
    Mcp.Input.none

let stay =
  Mcp.make ~name:"edit.stay" ~description:"Stay on the page and keep the draft."
    ~annotations:(Mcp.annotations ~destructive:false ~idempotent:true ())
    Mcp.Input.none

let cast =
  Mcp.make ~name:"vote.cast" ~description:"Cast one vote."
    ~annotations:(Mcp.annotations ~destructive:false ())
    Mcp.Input.none

let edit =
  Mcp.tools Example_edit.machine
    [
      Mcp.on draft (fun text -> Example_edit.Draft text);
      Mcp.on save (fun () -> Example_edit.Save);
      Mcp.on leave (fun () -> Example_edit.Leave);
      Mcp.on stay (fun () -> Example_edit.Stay);
    ]

let vote =
  Mcp.tools Example_vote_button.machine
    [ Mcp.on cast (fun () -> Remote_machine.Load ()) ]
let item = Mcp.tools Example_item.machine []
let publications = [ vote; edit; item ]
