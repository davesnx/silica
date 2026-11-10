let button =
  "rounded-ui bg-ui-accent px-3 py-1 text-sm text-ui-accent-text \
   hover:opacity-90 focus-visible:outline-2 focus-visible:outline-ui-focus \
   data-popup-open:opacity-80"

let dialog : Dialog.classes =
  {
    trigger = button;
    backdrop = "fixed inset-0 z-40 bg-ui-backdrop";
    popup =
      "fixed left-1/2 top-24 z-50 w-full max-w-md -translate-x-1/2 rounded-ui \
       border border-ui-border bg-ui-raised p-4 text-ui-text shadow-ui \
       outline-none";
    title = "text-lg font-semibold";
    description = "mt-1 text-sm text-ui-muted";
    close =
      "mt-3 rounded-ui px-3 py-1 text-sm text-ui-muted hover:bg-ui-highlight";
  }

let popover : Popover.classes =
  {
    trigger = button;
    popup =
      "z-50 w-72 rounded-ui border border-ui-border bg-ui-raised p-3 \
       text-ui-text shadow-ui outline-none";
    title = "text-sm font-semibold";
    close =
      "mt-2 rounded-ui px-3 py-1 text-sm text-ui-muted hover:bg-ui-highlight";
  }

let tooltip : Tooltip.classes =
  {
    trigger =
      "rounded-ui border border-ui-border px-3 py-1 text-sm \
       hover:bg-ui-highlight";
    popup =
      "z-50 w-max max-w-xs rounded-ui bg-ui-text px-2 py-1 text-xs \
       text-ui-surface shadow-ui";
  }

let menu : Menu.classes =
  {
    trigger = button;
    popup =
      "z-50 min-w-40 rounded-ui border border-ui-border bg-ui-raised p-1 \
       text-sm text-ui-text shadow-ui outline-none";
    item =
      "cursor-default select-none rounded-ui px-2 py-1 outline-none \
       data-highlighted:bg-ui-highlight data-disabled:text-ui-muted";
  }

let select : Select.classes =
  {
    trigger =
      "inline-flex min-w-40 items-center justify-between rounded-ui border \
       border-ui-border bg-ui-surface px-3 py-1 text-sm text-ui-text \
       data-placeholder:text-ui-muted";
    popup =
      "z-50 max-h-60 min-w-(--anchor-width) overflow-y-auto rounded-ui border \
       border-ui-border bg-ui-raised p-1 text-sm text-ui-text shadow-ui";
    option =
      "cursor-default select-none rounded-ui px-2 py-1 \
       data-highlighted:bg-ui-highlight data-selected:font-semibold \
       data-disabled:text-ui-muted";
  }

let field =
  "w-full rounded-ui border border-ui-border bg-ui-surface px-2 py-1 \
   text-ui-text outline-none focus:border-ui-focus"

let option =
  "cursor-default rounded-ui px-2 py-1 data-highlighted:bg-ui-highlight \
   data-selected:font-semibold data-disabled:text-ui-muted"

let combobox : Combobox.classes =
  {
    root = "w-64";
    input = field;
    list =
      "z-50 max-h-60 min-w-(--anchor-width) overflow-auto rounded-ui border \
       border-ui-border bg-ui-raised p-1 text-ui-text shadow-ui";
    option;
    empty = "px-2 py-1 text-sm text-ui-muted empty:hidden";
  }

let command_menu : Command_menu.classes =
  {
    dialog = { dialog with popup = dialog.popup ^ " flex flex-col gap-2" };
    combobox =
      {
        root = "flex flex-col gap-2";
        input = field;
        list = "max-h-72 overflow-auto";
        option;
        empty = "px-2 py-1 text-sm text-ui-muted empty:hidden";
      };
  }

let checkbox : Checkbox.classes =
  {
    root =
      "inline-flex items-center gap-2 text-sm text-ui-text disabled:opacity-50";
    indicator =
      "size-4 rounded-ui border border-ui-border bg-ui-surface \
       in-data-checked:border-ui-accent in-data-checked:bg-ui-accent \
       in-data-indeterminate:bg-ui-muted";
  }

let switch : Switch.classes =
  {
    root =
      "inline-flex items-center gap-2 text-sm text-ui-text disabled:opacity-50";
    track =
      "inline-flex h-5 w-9 items-center rounded-full bg-ui-border p-0.5 \
       in-data-checked:bg-ui-accent";
    thumb =
      "size-4 rounded-full bg-ui-surface shadow-ui transition-transform \
       data-checked:translate-x-4";
  }

let tabs : Tabs.classes =
  {
    list =
      "flex gap-1 border-b border-ui-border \
       data-[orientation=vertical]:flex-col";
    tab =
      "rounded-t-ui px-3 py-1 text-sm text-ui-muted hover:bg-ui-highlight \
       data-active:text-ui-text data-active:border-b-2 \
       data-active:border-ui-accent disabled:opacity-50";
  }

let tab_panel = "p-3 text-sm text-ui-text"

let toast : Toast.classes =
  {
    viewport =
      "fixed bottom-4 right-4 z-50 flex w-80 flex-col gap-2 outline-none";
    toast =
      "relative rounded-ui border border-ui-border bg-ui-raised p-3 pr-8 \
       text-ui-text shadow-ui data-[priority=high]:border-ui-accent";
    title = "text-sm font-semibold";
    description = "mt-1 text-sm text-ui-muted";
    close =
      "absolute right-2 top-2 rounded-ui px-1 text-ui-muted \
       hover:bg-ui-highlight";
  }
