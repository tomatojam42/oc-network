let show_help () =
  print_newline;

let get_connection arg =
  match arg with
  | "show" -> print_string ("NAME    UUID                                  
TYPE      DEVICE ");
    print_newline ();
    print_string ("LAN     
41ff4c10-3f39-3957-a6ab-099b3dedec71  ethernet  enp1s0");
    print_newline ();
    print_string ("virbr0  
d866c7aa-d7d6-4981-86cd-ee9e2358a82e  bridge    virbr0 ");
  | "help" -> print_newline ();

let main () =
  let arg1 = Sys.argv.(1) in
  let arg2 = Sys.argv.(2) in
  match arg1 with
    "help" -> show_help ()
    | "connection" -> get_connection arg2
  exit 0

main ()
