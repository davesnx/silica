(** Presses made on a page before its code runs.

    The server puts {!script} in the head of each page that loads scripts. Until
    the browser host mounts, it keeps each click on the page, except on links,
    form fields, labels and summaries (the browser does those itself), and each
    form submit made with the Enter key, which it stops. When the host has
    mounted and painted, it presses them again, in order, against the adopted
    page: at most 16, and none older than 10 seconds. *)

val script : string
(** The JavaScript of the script element, with no [<script>] tag. *)
