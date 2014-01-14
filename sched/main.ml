let simulate = ref false
let number_steps = ref (-1)
let sim_path = ref "./netlist_simulator.byte"
let dumb_down = ref false

let compile filename =
  try
    Format.printf("Reading netlist...%!");
    let p = Netlist.read_file filename in
    Format.printf("done.\n%!");

    let out_name = (Filename.chop_suffix filename ".net") ^ ".snet" in
    let dumb_out_name = (Filename.chop_suffix filename ".net") ^ ".dumb" in
    let out_opt_name = (Filename.chop_suffix filename ".net") ^ "_opt.snet" in
    let dumb_opt_out_name = (Filename.chop_suffix filename ".net") ^ "_opt.dumb" in
    let q, q_opt = ref p, ref p in

    begin try
      Format.printf("Topological sort on non-simplified netlist...%!");
      q := Scheduler.schedule p;
      Format.printf("done.\n%!");
      Format.printf("Dong simplifications...\n%!");
      q_opt := Simplify.simplify (!q);
      Format.printf("Simplifications done.\n%!")
    with
      | Scheduler.Combinational_cycle ->
          Format.eprintf "The netlist has a combinatory cycle.@.";
          exit 2;
    end;

    let out = open_out out_name in
    Netlist_printer.print_program out !q;
    close_out out;
    let dumb_out = open_out dumb_out_name in
    Netlist_dumb.print_program dumb_out !q;
    close_out dumb_out;

    let out_opt = open_out out_opt_name in
    Netlist_printer.print_program out_opt !q_opt;
    close_out out_opt;
    let dumb_opt_out = open_out dumb_opt_out_name in
    Netlist_dumb.print_program dumb_opt_out !q_opt;
    close_out dumb_opt_out;

    if !simulate then (
      let simulator =
        if !number_steps = -1 then
      !sim_path
        else
      !sim_path ^ " -n " ^ (string_of_int !number_steps)
      in
      ignore (Unix.system (simulator^" "^(if !dumb_down then dumb_out_name else out_name)))
    )
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-s", Arg.Set simulate, "Launch the simulator";
     "-sim", Arg.Set_string sim_path, "Path to the circuit simulator";
     "-d", Arg.Set dumb_down, "Pass the dumbed-down netlist to the simulator (for the C simulator)";
       "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
      compile
    ""

let () = main ()
