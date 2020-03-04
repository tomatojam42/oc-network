open Nm
module Nm_common = Nm_interfaces.Org_freedesktop_NetworkManager

module Nm_connection =
  Nm_interfaces.Org_freedesktop_NetworkManager_Connection_Active
module Nm_device = Nm_interfaces.Org_freedesktop_NetworkManager_Device
module Nm_settings = Nm_interfaces.Org_freedesktop_NetworkManager_Settings
module Nm_settings_conn =
  Nm_interfaces.Org_freedesktop_NetworkManager_Settings_Connection

(*open Ppx_lwt*)
(*open Lwt.Infix*)

let ( let* ) = Lwt.bind
let ( and* ) = Lwt.both

let lwt a = Lwt.return a

let open_prop prop proxy = OBus_property.get @@ OBus_property.make prop proxy

let show_help () = Lwt_io.printf "Commands: connection show, device show.\n"

let unwrap_string = function
  | OBus_value.V.Basic (OBus_value.V.String s) -> Some s
  | _ -> None

let make_proxy bus path =
  OBus_proxy.make
    ~peer:
      (OBus_peer.make ~connection:bus ~name:"org.freedesktop.NetworkManager")
    ~path

let rec parse lst f =
  match lst with
  | [] -> Lwt_io.printlf "Error"
  | [ x ] -> f x
  | hd :: tl ->
      let* () = f hd in
      parse tl f

let get_conn_props proxy prop =
  let* lst = OBus_method.call Nm_settings_conn.m_GetSettings proxy () in
  lwt @@
  try
    Option.value ~default:"--" @@ unwrap_string @@ List.assoc prop
    @@ List.assoc "connection" lst
  with Not_found -> "--"

let is_active bus path_to_setting =
  let nmproxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager" ] in
  let* act_conns = open_prop Nm_common.p_ActiveConnections nmproxy in
  let tr1 path_to_actconn =
    let proxy = make_proxy bus path_to_actconn in
    open_prop Nm_connection.p_Connection proxy
  in
  let rec parse lst =
    match lst with
    | [ x ] ->
        let* tr = tr1 x in
        if OBus_path.compare tr path_to_setting = 0 then lwt "+" else lwt "-"
    | hd :: tl ->
        let* tr = tr1 hd in
        if OBus_path.compare tr path_to_setting = 0 then lwt "+" else parse tl
    | [] -> lwt "-"
  in
  parse act_conns

let show_conn bus path =
  let proxy = make_proxy bus path in
  let* id = get_conn_props proxy "id" in
  let* uuid = get_conn_props proxy "uuid" in
  let* type_conn = get_conn_props proxy "type" in
  let* device = get_conn_props proxy "interface-name" in
  let* act = is_active bus path in
  Lwt_io.printlf "%s %s %s %s %s" act id uuid type_conn device

type connection = { id : string; uuid : string; path : OBus_path.t }

(*["org";"freedesktop";"NetworkManager";"Settings";"1"],["org";"freedesktop";"NetworkManager";"Devices";"2"],[]*)

let activate_conn path bus =
  let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager" ] in
  let* out_path =
    OBus_method.call Nm_common.m_ActivateConnection proxy (path, [], [])
  in
  Lwt_io.printl (String.concat "/" ("" :: out_path))

let deactivate_conn path bus =
  let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager" ] in
  let* () = OBus_method.call Nm_common.m_DeactivateConnection proxy path in
  Lwt_io.printl "Done"

let wrap_string x = OBus_value.V.basic_string x

let wrap_bool x = OBus_value.V.basic_boolean x

let wrap_int32 x = OBus_value.V.basic_int32 @@ Int32.of_int x

let reverse_int32 (x : int32) =
  let ( lsr ), ( lsl ), ( land ), ( + ) =
    Int32.shift_right_logical,
    Int32.shift_left,
    Int32.logand,
    Int32.add in
  let mask = 0xFFl in
  let (a,b,c,d) =
    ((x lsr 24) land mask),
    ((x lsr 16) land mask),
    ((x lsr 8) land mask),
    (x land mask) in
  ((d lsl 24) + (c lsl 16) + (b lsl 8) + a)

let wrap_dns_list x =
  List.map (fun dns -> OBus_value.V.basic_uint32 @@ reverse_int32 @@ (*Ipaddr.V4.to_int32*) Int32.of_int dns) x
  |> fun a -> (OBus_value.V.array (OBus_value.T.Basic OBus_value.T.Uint32) a)

let get_connection bus =
  let arg2 = Sys.argv.(2) in
  match arg2 with
  | "show" ->
      let* () =
        Lwt_io.print
          "? NAME    UUID                                  TYPE      DEVICE\n"
      in
      let proxy =
        make_proxy bus [ "org"; "freedesktop"; "NetworkManager"; "Settings" ]
      in
      let* conns = open_prop Nm_settings.p_Connections proxy in
      parse conns (show_conn bus)
  | "up" ->
      let input_name = Sys.argv.(3) in
      let uuid_reg = Str.regexp "........-....-....-....-............" in
      let path_reg = Str.regexp "/org/freedesktop/NetworkManager/Settings/*" in
      let proxy =
        make_proxy bus [ "org"; "freedesktop"; "NetworkManager"; "Settings" ]
      in
      let* conns = open_prop Nm_settings.p_Connections proxy in
      let rec parse conns label =
        match conns with
        | [] -> Lwt_io.printl "No connections"
        | [ x ] ->
            let proxy = make_proxy bus x in
            let* name = get_conn_props proxy label in
            if name = input_name then activate_conn x bus
            else Lwt_io.printlf "No connection with name \"%s\"" input_name
        | hd :: tl ->
            let proxy = make_proxy bus hd in
            let* name = get_conn_props proxy label in
            if name = input_name then activate_conn hd bus else parse tl label
      in
      if Str.string_match uuid_reg input_name 0 then parse conns "uuid"
      else if Str.string_match path_reg input_name 0 then
        let path = List.tl @@ String.split_on_char '/' input_name in
        activate_conn path bus
      else parse conns "id"
  | "down" ->
      let input_name = Sys.argv.(3) in
      let uuid_reg = Str.regexp "........-....-....-....-............" in
      let path_reg =
        Str.regexp "/org/freedesktop/NetworkManager/ActiveConnection/*"
      in
      let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager" ] in
      let* act_conns = open_prop Nm_common.p_ActiveConnections proxy in
      let rec parse conns prop =
        match conns with
        | [] -> Lwt_io.printl "No active connections"
        | [ x ] ->
            let proxy = make_proxy bus x in
            let* name = open_prop prop proxy in
            if name = input_name then deactivate_conn x bus
            else Lwt_io.printlf "No connection named %s" input_name
        | hd :: tl ->
            let proxy = make_proxy bus hd in
            let* name = open_prop prop proxy in
            if name = input_name then deactivate_conn hd bus else parse tl prop
      in
      if Str.string_match uuid_reg input_name 0 then
        parse act_conns Nm_connection.p_Uuid
      else if Str.string_match path_reg input_name 0 then
        let path = List.tl @@ String.split_on_char '/' input_name in
        deactivate_conn path bus
      else parse act_conns Nm_connection.p_Id
  | "add" -> 
      let* () = Lwt_io.printl "Введите имя:" in
      let* id = Lwt_io.read_line Lwt_io.stdin in
      let* () = Lwt_io.printl "Введите тип (например, 802-3-ethernet):" in
      let* type_conn = Lwt_io.read_line Lwt_io.stdin in
      let* () = Lwt_io.printl "Введите метод (auto или manual):" in
      let* method_conn = Lwt_io.read_line Lwt_io.stdin in
      let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager"; "Settings"] in
      
       if method_conn = "auto" then
       let* path = OBus_method.call Nm_settings.m_AddConnection proxy
       ["connection", [ "id",   wrap_string id; "type", wrap_string type_conn];
       "ipv4", ["method", wrap_string "auto"; "dns",  wrap_dns_list [134744072]]]
        in
       Lwt_io.printlf "%s" (String.concat "/" ("" :: path))

       else if method_conn = "manual" then
       let* () = Lwt_io.printl "Введите IP:" in
       let* ip_conn =  Lwt_io.read_line Lwt_io.stdin in
       let ip_conn2 = reverse_int32 @@ Ipaddr.V4.to_int32 @@ match (Ipaddr.V4.of_string ip_conn) with
       | Ok v -> v
       | Error _ -> failwith "err"
         in
       (*let* () = Lwt_io.printl "Введите маску подсети:" in
       let* mask_conn = Lwt_io.read_line Lwt_io.stdin in*)
       let aaa a = OBus_value.V.basic_uint32 a in
       let* path = let open OBus_value in
       OBus_method.call Nm_settings.m_AddConnection proxy
       ["connection", [ "id",   wrap_string id; "type", wrap_string type_conn];
       "ipv4", ["address-data", V.array (T.Dict (T.String, T.Variant)) [V.dict T.String T.Variant
       [V.string "adress", V.variant (wrap_string ip_conn); V.string "prefix", V.variant @@ V.basic_uint32 24l]];
       "addresses", V.array (T.Array (T.Basic T.Uint32)) [V.array (T.Basic T.Uint32) [aaa ip_conn2; aaa 24l; aaa 24094912l]];
       "method", wrap_string "manual"; "dns",  wrap_dns_list [134744072]]]
        in
        Lwt_io.printlf "%s" (String.concat "/" ("" :: path))
       else Lwt_io.printl "Неверный метод"
  | "help" ->
      Lwt_io.printlf
        "Type\n\
         \"oc_network connection show\" to show all your network connections;\n\
         \"oc_network connection up [id, uuid, path]\" to enable connection;\n\
         \"oc_network connection down [id, uuid, path]\" to disable connection."
  | _ ->
      Lwt_io.printlf "Maybe you want to type \"oc_network connection help\" ?"

let show_device bus path =
  let proxy = make_proxy bus path in
  let* device = open_prop Nm_device.p_Interface proxy in
  let* dev_type_lwt = open_prop Nm_device.p_DeviceType proxy in
  let dev_type =
    match dev_type_lwt with
    | 0l -> "unknown"
    | 1l -> "ethernet"
    | 2l -> "wifi"
    | 5l -> "bluetooth"
    | 8l -> "mobile modem"
    | 13l -> "bridge"
    | 14l -> "loopback"
    | 16l -> "tun"
    | 23l -> "PPP"
    | _ -> "unknown"
  in
  let* conn_path = open_prop Nm_device.p_ActiveConnection proxy in
  let* conn =
    if conn_path = [] then lwt "--"
    else open_prop Nm_connection.p_Id (make_proxy bus conn_path)
  in
  let* hwaddr =
    match dev_type with
    | "loopback" ->
        open_prop
          Nm_interfaces.Org_freedesktop_NetworkManager_Device_Generic
          .p_HwAddress proxy
    | "ethernet" ->
        open_prop
          Nm_interfaces.Org_freedesktop_NetworkManager_Device_Wired.p_HwAddress
          proxy
    | "bridge" ->
        open_prop
          Nm_interfaces.Org_freedesktop_NetworkManager_Device_Bridge.p_HwAddress
          proxy
    | "tun" ->
        open_prop
          Nm_interfaces.Org_freedesktop_NetworkManager_Device_Tun.p_HwAddress
          proxy
    | "wifi" ->
        open_prop
          Nm_device_wireless_interfaces
          .Org_freedesktop_NetworkManager_Device_Wireless
          .p_HwAddress proxy
    | _ -> lwt "--"
  in
  Lwt_io.printf "DEVICE: %s\nTYPE: %s\nHWADDR: %s\nCONNECTION: %s\n\n" device
    dev_type hwaddr conn

let get_device () =
  let arg = Sys.argv.(2) in
  match arg with
  | "show" ->
      let* bus = OBus_bus.system () in
      let proxy = make_proxy bus [ "org"; "freedesktop"; "NetworkManager" ] in
      let* devices = open_prop Nm_common.p_Devices proxy in
      parse devices (show_device bus)
  | "help" ->
      Lwt_io.printlf
        "Type \"oc_network device show\" to show all your network devices."
  | _ -> Lwt_io.printlf "Maybe you want to type \"oc_network device help\" ?"

let main () =
  Lwt_main.run
    ( try
        let arg1 = Sys.argv.(1) in
        let* bus = OBus_bus.system () in
        match arg1 with
        | "help" -> show_help ()
        | "connection" -> get_connection bus
        | "device" -> get_device ()
        | _ -> show_help ()
      with Invalid_argument _ -> show_help () )

let () = main ()