(* SIMPLIFICATION PASSES *)

(*
  Order of simplifications :
  - cascade slices and selects
  - transform k = SLICE i j var when var = CONCAT var' var''
  - simplify stupid things (a xor 0 = a, a and 0 = 0, etc.)
    transform k = SLICE i i var into k = SELECT i var
  - transform k = SELECT 0 var into k = var when var is also one bit
  - look for variables with same equation, put the second to identity
  - eliminate k' for each equation k' = k
  - topological sort

  TODO : eliminate unused variables. problem : they are hard to identify
*)

open Netlist_ast

module Sset = Set.Make(String)
module Smap = Map.Make(String)

(* Simplify cascade slicing/selecting *)
let cascade_slices p =
  let usefull = ref false in
  let slices = Hashtbl.create (Env.cardinal p.p_vars / 12) in
  let eqs_new = List.map
    (fun (n, eq) -> (n, match eq with
      | Eslice(u, v, Avar(x)) ->
        let dec, nx =
          if Hashtbl.mem slices x then begin
            Hashtbl.find slices x
          end else 
             (0, x)
        in
        Hashtbl.add slices n (u + dec, nx);
        if nx <> x || dec <> 0 then usefull := true;
        Eslice(u + dec, v + dec, Avar(nx))
      | Eselect(u, Avar(x)) ->
        begin try
          let ku, kx = Hashtbl.find slices x in
          usefull := true;
          Eselect(ku + u, Avar(kx))
        with
          Not_found -> Eselect(u, Avar(x))
        end
      | _ -> eq))
    p.p_eqs in
  {
    p_eqs = eqs_new;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars;
  }, !usefull

(* If
  var = CONCAT a b
  x = SLICE i j var
  or
  y = SELECT i var
  then x or y may be simplified
*)
let pass_concat p =
  let usefull = ref false in
  let concats = Hashtbl.create (Env.cardinal p.p_vars / 12) in
  List.iter (fun (n, eq) -> match eq with
      | Econcat(x, y) ->
        let s1 = match x with
          | Aconst(a) -> Array.length a
          | Avar(z) -> Env.find z p.p_vars
        in let s2 = match y with
          | Aconst(a) -> Array.length a
          | Avar(z) -> Env.find z p.p_vars
        in
        Hashtbl.add concats n (x, s1, y, s2)
      | _ -> ()) p.p_eqs;
  let eqs_new = List.map
    (fun (n, eq) -> (n, match eq with
      | Eselect(i, Avar(n)) ->
        begin try
          let (x, s1, y, s2) = Hashtbl.find concats n in
          usefull := true;
          if i < s1 then
            Eselect(i, x)
          else
            Eselect(i-s1, y)
        with Not_found -> eq end
      | Eslice(i, j, Avar(n)) ->
        begin try
          let (x, s1, y, s2) = Hashtbl.find concats n in
          if j < s1 then begin
            usefull := true;
            Eslice(i, j, x)
          end else if i >= s1 then begin
            usefull := true;
            Eslice(i - s1, j - s1, y)
          end else eq
        with Not_found -> eq end
      | _ -> eq))
    p.p_eqs in
  {
    p_eqs = eqs_new;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars;
  }, !usefull
        

(* Simplifies some trivial arithmetic possibilites :
  a and 1 = a
  a and 0 = 0
  a or 1 = 1
  a or 0 = a
  a xor 0 = a
  slice i i x = select i x
  concat const const = const.const
  slice i j const = const.[i..j]
  select i const = const.[i]
*)
let arith_simplify p =
  let usefull = ref false in
  {
    p_eqs = List.map
      (fun (n, eq) ->
      let useless = ref false in
      let neq = match eq with
      | Ebinop(Or, Aconst([|false|]), x) -> Earg(x)
      | Ebinop(Or, Aconst([|true|]), x) -> Earg(Aconst([|true|]))
      | Ebinop(Or, x, Aconst([|false|])) -> Earg(x)
      | Ebinop(Or, x, Aconst([|true|])) -> Earg(Aconst([|true|]))

      | Ebinop(And, Aconst([|false|]), x) -> Earg(Aconst([|false|]))
      | Ebinop(And, Aconst([|true|]), x) -> Earg(x)
      | Ebinop(And, x, Aconst([|false|])) -> Earg(Aconst([|false|]))
      | Ebinop(And, x, Aconst([|true|])) -> Earg(x)

      | Ebinop(Xor, Aconst([|false|]), x) -> Earg(x)
      | Ebinop(Xor, x, Aconst([|false|])) -> Earg(x)

      | Ebinop(Nand, Avar(a), Avar(b)) when a = b ->
        Enot(Avar(a))
      | Ebinop(Xor, Avar(a), Avar(b)) when a = b ->
        let sz = Env.find a p.p_vars in
        Earg(Aconst(Array.make sz false))
      | Ebinop(_, Avar(a), Avar(b)) when a = b ->
        Earg(Avar(a))
      
      | Emux(_, a, b) when a = b -> Earg(a)
      | Emux(Aconst[|false|], a, b) -> Earg(a)    
      | Emux(Aconst[|true|], a, b) -> Earg(b)

      | Eslice(i, j, k) when i = j -> Eselect(i, k)

      | Econcat(Aconst(a), Aconst(b)) ->
        Earg(Aconst(Array.append a b))
      
      | Eslice(i, j, Aconst(a)) ->
        Earg(Aconst(Array.sub a i (j - i + 1)))
      
      | Eselect(i, Aconst(a)) ->
        Earg(Aconst([|a.(i)|]))
      
      | _ ->  useless := true; eq in
      if not !useless then usefull := true;
      (n, neq))
      p.p_eqs;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars;
  }, !usefull

(* if x is one bit, then :
  select 0 x = x
  and same thing with select
*)
let select_to_id p =
  let usefull = ref false in
  {
    p_eqs = List.map
      (fun (n, eq) -> match eq with
      | Eselect(0, Avar(id)) when Env.find id p.p_vars = 1 ->
        usefull := true;
        (n, Earg(Avar(id)))
      | Eslice(0, sz, Avar(id)) when Env.find id p.p_vars = sz + 1 ->
        usefull := true;
        (n, Earg(Avar(id)))
      | _ -> (n, eq))
      p.p_eqs;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars;
  }, !usefull

(*
  If a = eqn(v1, v2, ...) and b = eqn(v1, v2, ...)   <- the same equation
  then say b = a
*)
let same_eq_simplify p =
  let usefull = ref false in
  let id_outputs =
    (List.fold_left (fun x k -> Sset.add k x) Sset.empty p.p_outputs) in
  let eq_map = Hashtbl.create (List.length p.p_eqs) in
  List.iter
    (fun (n, eq) -> if Sset.mem n id_outputs then
      Hashtbl.add eq_map eq n)
    p.p_eqs;
  let simplify_eq (n, eq) =
    if Sset.mem n id_outputs then
      (n, eq)
    else if Hashtbl.mem eq_map eq then begin
      usefull := true;
      (n, Earg(Avar(Hashtbl.find eq_map eq)))
    end else begin
      Hashtbl.add eq_map eq n;
      (n, eq)
    end
  in
  let eq2 = List.map simplify_eq p.p_eqs in
  {
    p_eqs = eq2;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = p.p_vars;
  }, !usefull


(*  Replace one specific variable by another argument in the arguments of all equations
  (possibly a constant, possibly another variable)
*)
let eliminate_var var rep p =
  let rep_arg = function
    | Avar(i) when i = var -> rep
    | k -> k
  in
  let rep_eqs = List.map
    (fun (n, eq) -> (n, match eq with
      | Earg(a) -> Earg(rep_arg a)
      | Ereg(i) when i = var ->
        begin match rep with
        | Avar(j) -> Ereg(j)
        | Aconst(k) -> Earg(Aconst(k))
        end
      | Ereg(j) -> Ereg(j)
      | Enot(a) -> Enot(rep_arg a)
      | Ebinop(o, a, b) -> Ebinop(o, rep_arg a, rep_arg b)
      | Emux(a, b, c) -> Emux(rep_arg a, rep_arg b, rep_arg c)
      | Erom(u, v, a) -> Erom(u, v, rep_arg a)
      | Eram(u, v, a, b, c, d) -> Eram(u, v, rep_arg a, rep_arg b, rep_arg c, rep_arg d)
      | Econcat(a, b) -> Econcat(rep_arg a, rep_arg b)
      | Eslice(u, v, a) -> Eslice(u, v, rep_arg a)
      | Eselect(u, a) -> Eselect(u, rep_arg a)
      ))
    p.p_eqs in
  {
    p_eqs = List.fold_left
      (fun x (n, eq) ->
        if n = var then x else (n, eq)::x)
      [] rep_eqs;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = Env.remove var p.p_vars;
  }

(* Remove all equations of type :
  a = b
  a = const
  (except if a is an output variable)
*)
let rec eliminate_id p =
  let id_outputs =
    (List.fold_left (fun x k -> Sset.add k x) Sset.empty p.p_outputs) in

  let rep =
    List.fold_left
      (fun x (n, eq) ->
        if x = None && (not (Sset.mem n id_outputs)) then
          match eq with
          | Earg(rarg) -> 
            Some(n, rarg)
          | _ -> None
        else
          x)
      None p.p_eqs in
  match rep with
  | None -> p, false
  | Some(n, rep) -> fst (eliminate_id (eliminate_var n rep p)), true

(* Eliminate dead variables *)
let eliminate_dead p =
  let rec living basis =
    let new_basis = List.fold_left
      (fun b2 (n, eq) ->
        if Sset.mem n b2 then
          List.fold_left
            (fun x k -> Sset.add k x)
            b2
            (Scheduler.read_exp_all eq)
        else
          b2)
      basis (List.rev p.p_eqs)
    in
    if Sset.cardinal new_basis > Sset.cardinal basis
      then living new_basis
      else new_basis
  in
  let outs = List.fold_left (fun x k -> Sset.add k x) Sset.empty p.p_outputs in
  let ins = List.fold_left (fun x k -> Sset.add k x) Sset.empty p.p_inputs in
  let live = living (Sset.union outs ins) in
  {
    p_eqs = List.filter (fun (n, _) -> Sset.mem n live) p.p_eqs;
    p_inputs = p.p_inputs;
    p_outputs = p.p_outputs;
    p_vars = Env.fold
      (fun k s newenv -> 
        if Sset.mem k live
          then Env.add k s newenv
          else newenv)
      p.p_vars Env.empty
  }, (Sset.cardinal live < Env.cardinal p.p_vars)

(* Topological sort *)
let topo_sort p =
  (Scheduler.schedule p, false)


(* Apply all the simplification passes,
  in the order given in the header of this file
*)
let rec simplify_with steps p =
  let pp, use = List.fold_left
    (fun (x, u) (f, n) ->
      Format.printf "%s...%!" n;
      let xx, uu = f x in 
      Format.printf "%s\n%!" (if uu then " *" else "");
      (xx, u || uu))
    (p, false) steps in
  if use then simplify_with steps pp else pp

let simplify p =
  let p = simplify_with [
    cascade_slices, "cascade_slices";
    pass_concat, "pass_concat";
    select_to_id, "select_to_id";
    arith_simplify, "arith_simplify";
  ] p in
  let p = simplify_with [
    arith_simplify, "arith_simplify";
    same_eq_simplify, "same_eq_simplify"; 
    eliminate_id, "eliminate_id";
  ] p in
  let p = simplify_with [
    cascade_slices, "cascade_slices";
    pass_concat, "pass_concat";
    arith_simplify, "arith_simplify";
    select_to_id, "select_to_id";
    same_eq_simplify, "same_eq_simplify"; 
    eliminate_id, "eliminate_id";
  ] p in
  let p = simplify_with [
    eliminate_dead, "eliminate_dead";
    topo_sort, "topo_sort"; (* make sure last step is a topological sort *)
  ] p in
  p

