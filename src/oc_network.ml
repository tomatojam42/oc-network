open Nm
module Nm_common = Nm_interfaces.Org_freedesktop_NetworkManager
module Nm_connection = Nm_interfaces.Org_freedesktop_NetworkManager_Connection_Active
module Nm_device = Nm_interfaces.Org_freedesktop_NetworkManager_Device
(*open Ppx_lwt*)
let (let*) = Lwt.bind
let (and*) = Lwt.both
(*open Lwt.Infix*)
let lwt a = Lwt.return a

let open_prop prop proxy =
  OBus_property.get @@ OBus_property.make prop proxy

let show_help () = Lwt_io.printf "This will be the help.\n"

let make_proxy bus path =
  OBus_proxy.make
    ~peer:(OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
    ~path
(*[ "org"; "freedesktop"; "NetworkManager"; "ActiveConnection"; "2" ]*)

let show_conn bus path =
  let proxy = make_proxy bus path in
  let* id = open_prop Nm_connection.p_Id proxy in
  let* uuid = open_prop Nm_connection.p_Uuid proxy in
  let* type_conn = open_prop Nm_connection.p_Type proxy in
  let* dev_path1 = open_prop Nm_connection.p_Devices proxy in
  let dev_path = match dev_path1 with [x] -> x |_ -> [] in
  let* device = if dev_path = [] then lwt "--" else open_prop Nm_device.p_Interface (make_proxy bus dev_path) in
  Lwt_io.printlf "%s %s %s %s" id uuid type_conn device

let get_connection () =
  let arg2 = Sys.argv.(2) in
  match arg2 with
  | "show" ->
        let _ = Lwt_io.print
        "NAME    UUID                                  TYPE      DEVICE\n" in
        let* bus = OBus_bus.system () in
        let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager"] in
        let* act_conns = open_prop Nm_common.p_ActiveConnections proxy in
        let rec parse lst =
          match lst with
          | [] -> Lwt_io.printlf "There is no connections"
          | [x] -> show_conn bus x
          | hd::tl -> let _ = show_conn bus hd in parse tl in
        parse act_conns
  | "help" -> Lwt_io.printlf "connection help"
  | _ -> Lwt_io.printlf "soooooomething"
  

let show_device bus path =
  let proxy = make_proxy bus path in
  let* device = open_prop Nm_device.p_Interface proxy in
  let* dev_type_lwt = open_prop Nm_device.p_DeviceType proxy in
  let dev_type = 
    match dev_type_lwt with
    | 0l -> "unknown"
    | 1l -> "ethernet"
    | 2l -> "WiFi"
    | 5l -> "bluetooth"
    | 8l -> "mobile modem"
    | 13l -> "bridge"
    | 14l -> "loopback"
    | 16l -> "tun"
    | 23l -> "PPP"
    | _ -> "unknown"
    in
  let* conn_path = open_prop Nm_device.p_ActiveConnection proxy in
  let* conn = if conn_path = [] then lwt "--" else open_prop Nm_connection.p_Id (make_proxy bus conn_path) in
  let* hwaddr = 
    match dev_type with
    | "loopback" -> open_prop Nm_interfaces.Org_freedesktop_NetworkManager_Device_Generic.p_HwAddress proxy
    | "ethernet" -> open_prop Nm_interfaces.Org_freedesktop_NetworkManager_Device_Wired.p_HwAddress proxy
    | "bridge" -> open_prop Nm_interfaces.Org_freedesktop_NetworkManager_Device_Bridge.p_HwAddress proxy
    | "tun" -> open_prop Nm_interfaces.Org_freedesktop_NetworkManager_Device_Tun.p_HwAddress proxy
    | _ -> lwt "something"
    in
  Lwt_io.printf
        "DEVICE: %s\n\
         TYPE: %s\n\
         HWADDR: %s\n\
         CONNECTION: %s\n\n" device dev_type hwaddr conn


let get_device () =
  let arg = Sys.argv.(2) in
  match arg with
  | "show" ->
         let* bus = OBus_bus.system () in
         let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager"] in
         let* devices = open_prop Nm_common.p_Devices proxy in
         let rec parse lst =
           match lst with
           | [] -> Lwt_io.printlf "There is no network devices"
           | [x] -> show_device bus x
           | hd::tl -> let _ = show_device bus hd in parse tl in
         parse devices
  | "help" -> Lwt_io.printlf "Device help"
  | _ -> Lwt_io.printlf "Device help"

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
