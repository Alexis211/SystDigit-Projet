open Netlist_ast

module Smap = Map.Make(String)

exception Combinational_cycle

let add_arg x l = match x with
  | Avar(f) -> f::l
  | Aconst(_) -> l

let read_exp eq =
  let aux = function
  | Earg(x) -> add_arg x []
  | Ereg(i) -> []
  | Enot(x) -> add_arg x []
  | Ebinop(_, x, y) -> add_arg x (add_arg y [])
  | Emux(a, b, c) -> add_arg a (add_arg b (add_arg c []))
  | Erom(_, _, a) -> add_arg a []
  | Eram(_, _, ra, wa, we, d) -> add_arg ra []
  | Econcat(u, v) -> add_arg u (add_arg v [])
  | Eslice(_, _, a) -> add_arg a []
  | Eselect(_, a) -> add_arg a []
  in
    aux eq

let read_exp_all eq =
  let aux = function
  | Earg(x) -> add_arg x []
  | Ereg(i) -> [i]
  | Enot(x) -> add_arg x []
  | Ebinop(_, x, y) -> add_arg x (add_arg y [])
  | Emux(a, b, c) -> add_arg a (add_arg b (add_arg c []))
  | Erom(_, _, ra) -> add_arg ra []
  | Eram(_, _, ra, wa, we, d) -> add_arg ra (add_arg wa (add_arg we (add_arg d [])))
  | Econcat(u, v) -> add_arg u (add_arg v [])
  | Eslice(_, _, a) -> add_arg a []
  | Eselect(_, a) -> add_arg a []
  in
    aux eq

let prog_eq_map p =
  List.fold_left
    (fun x (vn, eqn) -> Smap.add vn eqn x)
    Smap.empty p.p_eqs

let prog_graph p eq_map =
  let graph = Graph.mk_graph() in
  (* Add variables as graph nodes *)
  List.iter (fun (k, _) -> Graph.add_node graph k) p.p_eqs;
  (* Add dependencies as graph edges *)
  List.iter 
    (fun (n, e) -> List.iter
      (fun m -> if Smap.mem m eq_map then Graph.add_edge graph m n else ())
      (read_exp e))
    p.p_eqs;
  (* Verify there are no cycles *)
  if Graph.has_cycle graph then raise Combinational_cycle;
  graph

let schedule p =
  let eq_map = prog_eq_map p in
  let graph = prog_graph p eq_map in

  (* Topological sort of graph nodes *)
  let topo_vars = Graph.topological graph in
  (* Construct new program with sorted expression list *)
  {
    p_eqs = List.fold_right
      (fun n x ->
        if Smap.mem n eq_map then
          (n, Smap.find n eq_map)::x
        else x)
      topo_vars [];
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars;
  }



