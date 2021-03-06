(* File auto-generated by obus-gen-interface, DO NOT EDIT. *)
open OBus_value
open OBus_value.C
open OBus_member
open OBus_object
module Org_freedesktop_NetworkManager_Device_Wireless =
struct
  let interface = "org.freedesktop.NetworkManager.Device.Wireless"
  let m_GetAccessPoints = {
    Method.interface = interface;
    Method.member = "GetAccessPoints";
    Method.i_args = (arg0);
    Method.o_args = (arg1
                       (Some "access_points", array basic_object_path));
    Method.annotations = [];
  }
  let m_GetAllAccessPoints = {
    Method.interface = interface;
    Method.member = "GetAllAccessPoints";
    Method.i_args = (arg0);
    Method.o_args = (arg1
                       (Some "access_points", array basic_object_path));
    Method.annotations = [];
  }
  let m_RequestScan = {
    Method.interface = interface;
    Method.member = "RequestScan";
    Method.i_args = (arg1
                       (Some "options", dict string variant));
    Method.o_args = (arg0);
    Method.annotations = [];
  }
  let s_AccessPointAdded = {
    Signal.interface = interface;
    Signal.member = "AccessPointAdded";
    Signal.args = (arg1
                       (Some "access_point", basic_object_path));
    Signal.annotations = [];
  }
  let s_AccessPointRemoved = {
    Signal.interface = interface;
    Signal.member = "AccessPointRemoved";
    Signal.args = (arg1
                       (Some "access_point", basic_object_path));
    Signal.annotations = [];
  }
  let s_PropertiesChanged = {
    Signal.interface = interface;
    Signal.member = "PropertiesChanged";
    Signal.args = (arg1
                       (Some "properties", dict string variant));
    Signal.annotations = [];
  }
  let p_AccessPoints = {
    Property.interface = interface;
    Property.member = "AccessPoints";
    Property.typ = array basic_object_path;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  let p_ActiveAccessPoint = {
    Property.interface = interface;
    Property.member = "ActiveAccessPoint";
    Property.typ = basic_object_path;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  let p_Bitrate = {
    Property.interface = interface;
    Property.member = "Bitrate";
    Property.typ = basic_uint32;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  let p_HwAddress = {
    Property.interface = interface;
    Property.member = "HwAddress";
    Property.typ = basic_string;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  let p_LastScan = {
    Property.interface = interface;
    Property.member = "LastScan";
    Property.typ = basic_int64;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  let p_Mode = {
    Property.interface = interface;
    Property.member = "Mode";
    Property.typ = basic_uint32;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  let p_PermHwAddress = {
    Property.interface = interface;
    Property.member = "PermHwAddress";
    Property.typ = basic_string;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  let p_WirelessCapabilities = {
    Property.interface = interface;
    Property.member = "WirelessCapabilities";
    Property.typ = basic_uint32;
    Property.access = Property.readable;
    Property.annotations = [];
  }
  type 'a members = {
    m_GetAccessPoints : 'a OBus_object.t -> unit -> OBus_path.t list Lwt.t;
    m_GetAllAccessPoints : 'a OBus_object.t -> unit -> OBus_path.t list Lwt.t;
    m_RequestScan : 'a OBus_object.t -> (string * OBus_value.V.single) list -> unit Lwt.t;
    p_AccessPoints : 'a OBus_object.t -> OBus_path.t list React.signal;
    p_ActiveAccessPoint : 'a OBus_object.t -> OBus_path.t React.signal;
    p_Bitrate : 'a OBus_object.t -> int32 React.signal;
    p_HwAddress : 'a OBus_object.t -> string React.signal;
    p_LastScan : 'a OBus_object.t -> int64 React.signal;
    p_Mode : 'a OBus_object.t -> int32 React.signal;
    p_PermHwAddress : 'a OBus_object.t -> string React.signal;
    p_WirelessCapabilities : 'a OBus_object.t -> int32 React.signal;
  }
  let make members =
    OBus_object.make_interface_unsafe interface
      [
        ("org.gtk.GDBus.C.Name", "DeviceWifi");
      ]
      [|
        method_info m_GetAccessPoints members.m_GetAccessPoints;
        method_info m_GetAllAccessPoints members.m_GetAllAccessPoints;
        method_info m_RequestScan members.m_RequestScan;
      |]
      [|
        signal_info s_AccessPointAdded;
        signal_info s_AccessPointRemoved;
        signal_info s_PropertiesChanged;
      |]
      [|
        property_r_info p_AccessPoints members.p_AccessPoints;
        property_r_info p_ActiveAccessPoint members.p_ActiveAccessPoint;
        property_r_info p_Bitrate members.p_Bitrate;
        property_r_info p_HwAddress members.p_HwAddress;
        property_r_info p_LastScan members.p_LastScan;
        property_r_info p_Mode members.p_Mode;
        property_r_info p_PermHwAddress members.p_PermHwAddress;
        property_r_info p_WirelessCapabilities members.p_WirelessCapabilities;
      |]
end
