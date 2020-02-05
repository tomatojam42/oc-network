open Nm
module Nm_common = Nm_interfaces.Org_freedesktop_NetworkManager
module Nm_connection = Nm_interfaces.Org_freedesktop_NetworkManager_Connection_Active
(*open Ppx_lwt*)
let (let*) = Lwt.bind
let (and*) = Lwt.both
(*open Lwt.Infix*)

let show_help () = Lwt_io.printf "This will be the help.\n"

let make_proxy bus path =
  OBus_proxy.make
    ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
    ~path
(*[ "org"; "freedesktop"; "NetworkManager"; "ActiveConnection"; "2" ]*)
let get_connection () =
  let arg2 = Sys.argv.(2) in
  match arg2 with
  | "show" ->
        let _ = Lwt_io.print 
        "NAME    UUID                                  TYPE      DEVICE\n" in
        let* bus = OBus_bus.system () in
        let proxy = make_proxy bus  in
        let* id = OBus_property.get @@ OBus_property.make Nm_connection.p_Id proxy in
        let* uuid = OBus_property.get @@ OBus_property.make Nm_connection.p_Uuid proxy in
        let* type_conn = OBus_property.get @@ OBus_property.make Nm_connection.p_Type proxy in
        Lwt_io.printlf "%s %s %s" id uuid type_conn
  | "help" -> Lwt_io.printlf "connection help"
  | _ -> Lwt_io.printlf "soooooomething"
  

let get_device () =
  let arg = Sys.argv.(2) in
  match arg with
  | "show" ->
  Lwt_io.printf
        "DEVICE: enp1s0\n\
         TYPE: ethernet\n\
         HWADDR: 20:CF:30:8B:59:C4\n\
         CONNECTION: LAN\n"
  | "help" -> Lwt_io.printlf ""
  | _ -> Lwt_io.printlf ""

let main () =
  Lwt_main.run begin 
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
    show_help ()
  end
let () = main ()
