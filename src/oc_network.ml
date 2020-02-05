open Nm
module Nm_common = Nm_interfaces.Org_freedesktop_NetworkManager
module Nm_connection = Nm_interfaces.Org_freedesktop_NetworkManager_Connection_Active
(*open Ppx_lwt*)
let (let*) = Lwt.bind
let (and*) = Lwt.both


let show_help () = print_string "This will be the help.\n"

let make_proxy bus path =
  OBus_proxy.make
    ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
    ~path

let get_connection () =
  Lwt_main.run begin 
    let* bus = OBus_bus.system () in
    let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager"; "ActiveConnection"; "2" ] in
    let* id = OBus_property.get @@ OBus_property.make Nm_connection.p_Id proxy in
    let* uuid = OBus_property.get @@ OBus_property.make Nm_connection.p_Uuid proxy in
    let* type_conn = OBus_property.get @@ OBus_property.make Nm_connection.p_Type proxy in
    Lwt_io.printlf "%s %s %s\n" id uuid type_conn
  end;
  let arg2 = Sys.argv.(2) in
  match arg2 with
  | "show" ->
      print_string
        "NAME    UUID                                  TYPE      DEVICE\n";
      print_string
        "LAN     41ff4c10-3f39-3957-a6ab-099b3dedec71  ethernet  enp1s0\n";
      print_string
        "virbr0  d866c7aa-d7d6-4981-86cd-ee9e2358a82e  bridge    virbr0\n"
  | "help" -> print_newline ()
  | _ -> print_newline ()
  

let get_device () =
  let arg = Sys.argv.(2) in
  match arg with
  | "show" ->
      print_string
        "DEVICE: enp1s0\n\
         TYPE: ethernet\n\
         HWADDR: 20:CF:30:8B:59:C4\n\
         CONNECTION: LAN\n"
  | "help" -> print_newline ()
  | _ -> print_newline ()

let main () =
  try
    let arg1 = Sys.argv.(1) in
    match arg1 with
    | "help" ->
        show_help ()
    | "connection" ->
        get_connection ()
    | "device" ->
        get_device ()
    | _ -> show_help ()
  with Invalid_argument _ ->
    print_newline ()

let () = main ()
