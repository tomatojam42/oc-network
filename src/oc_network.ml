module Nm_common = Nm.Nm_common_interfaces.Org_freedesktop_NetworkManager

let show_help () = print_string "This will be the help.\n"

let get_connection arg =
  match arg with
  | "show" ->
      print_string
        "NAME    UUID                                  TYPE      DEVICE\n";
      print_string
        "LAN     41ff4c10-3f39-3957-a6ab-099b3dedec71  ethernet  enp1s0\n";
      print_string
        "virbr0  d866c7aa-d7d6-4981-86cd-ee9e2358a82e  bridge    virbr0\n"
  | "help" -> print_newline ()
  | _ -> print_newline ()

let get_device arg =
  match arg with
  | "show" ->
      print_string
        "DEVICE: enp1s0\n\
         TYPE: ethernet\n\
         HWADDR: 20:CF:30:8B:59:C4\n\
         CONNECTION: LAN\n"
  | "help" -> print_newline ()
  | _ -> print_newline ()
(*  | _:string -> print_string ("error"); print_newline;;*)

let main () =
  try
    let arg1 = Sys.argv.(1) in
    let arg2 = Sys.argv.(2) in
    match arg1 with
    | "help" ->
        show_help ()
    | "connection" ->
        get_connection arg2
    | "device" ->
        get_device arg2
    | _ -> show_help ()
  with Invalid_argument _ ->
    show_help ()

let () = main ()
