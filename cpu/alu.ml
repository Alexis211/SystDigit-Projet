open Netlist_gen

let zeroes n =
    const (String.make n '0')

let one n =
    const "1" ++ zeroes (n-1)
let two n =
    const "01" ++ zeroes (n-2)

let rec rep n k =
    if n = 1 then k
    else
        let s = rep (n/2) k in
        if n mod 2 = 0 then s ++ s else s ++ s ++ k

let rec eq_c n v c = (* v is a value, c is a constant *)
    if n = 1 then
        if c = 1 then v else not v
    else
        if c mod 2 = 1 then
            mux (v ** 0) (const "0") (eq_c (n-1) (v % (1, n-1)) (c/2))
        else
            mux (v ** 0) (eq_c (n-1) (v % (1, n-1)) (c/2)) (const "0")

let rec all1 n x =
    if n = 1 then
        x
    else
        (x ** 0) ^& (all1 (n-1) (x % (1, n-1)))

let rec nonnull n a =
    if n = 1 then
        a
    else
        (a ** 0) ^| (nonnull (n-1) (a % (1, n-1)))

let rec sign_extend n_a n_dest a =
    a ++ rep (n_dest - n_a) (a ** (n_a - 1))

(* Arithmetic operations *)

let fulladder a b c =
       let s = a ^^ b ^^ c in
       let r = (a ^& b) ^| ((a ^^ b) ^& c) in
       s, r

let rec nadder_with_carry n a b c_in =
    if n = 1 then fulladder a b c_in
    else 
        let s_n, c_n1 = fulladder (a ** 0) (b ** 0) c_in in
        let s_n1, c_out = nadder_with_carry (n-1) (a % (1, n-1)) (b % (1, n-1)) c_n1 in
        s_n ++ s_n1, c_out

let nadder n a b =
    let a, b = nadder_with_carry n a b (const "0") in
    b ^. a

let neg n a = nadder n (not a) (one n)

let rec nsubber n a b =
  let r, c = nadder_with_carry n a (not b) (const "1") in
  c ^. r




(* Comparisons *)

let rec eq_n n a b =
    all1 n (not (a ^^ b))

let rec ne_n n a b =
    nonnull n (a ^^ b)

let rec lt_n n a b =
    const "0"       (* TODO : less than *)

let rec ult_n n a b =
    const "0"       (* TODO : less than, unsigned *)

let rec le_n n a b =
    const "0"       (* TODO : less than or equal *)

let rec ule_n n a b =
  if n = 0 then const "1" else
    mux ((eq_c 1 (a ** (n-1)) 1) ^& (eq_c 1 (b ** (n-1)) 0))
      (mux ((eq_c 1 (a ** (n-1)) 0) ^& (eq_c 1 (b ** (n-1)) 1))
         (ule_n (n-1) (a % (0, n-2)) (b % (0, n-2)))
         (const "1")
      )
      (const "0")





(* Some operations on Redundant Binary Representation 
   Each binary digit is encoded on 2 bits 

   A n-digits number in RBR is written
   [a_0, a'_0, a_1, a'_1, ..., a_(n-1), a'_(n-1)]

*)

(* [a] and [b] are encoded on 2n bits
   [c_in] and [c_out] on 2 bits *)

let rec rbr_nadder_with_carry n a b c_in =

  if n = 0 then (zeroes 0), c_in else

  let fa1s, fa1r = fulladder (a ** 1) (b ** 0) (b ** 1) in
  let fa2s, fa2r = fulladder (c_in ** 1) (a ** 0) fa1s in

  let rec_s, rec_c = 
    rbr_nadder_with_carry (n - 1) 
      (a % (2, 2*n - 1)) 
      (b % (2, 2*n - 1)) 
      (fa1r ++ fa2r)

  in (c_in ** 0) ++ fa2s ++ rec_s, rec_c


let rbr_nadder n a b = 
  let s, c = rbr_nadder_with_carry n a b (zeroes 2) in
  c ^. s


let bin_of_rbr n a c = 

  (* Split even and odd bits *)
  let rec split_bits n a =
    if n = 0 then (zeroes 0, zeroes 0) 
    else
      let even, odd = split_bits (n-1) (a % (2, 2*n - 1)) in
      (a ** 0) ++ even, (a ** 1) ++ odd

  in
  let a_even, a_odd = split_bits n a in

  nadder n a_even a_odd
    
    

(* TODO : move to utils module *)
let rec range a b = if a > b then [] else a :: (range (a+1) b)

(* Sépare en deux listes de même taille une liste de taille paire *)
let rec split_list = function
  | [] -> [], []
  | [_] -> assert false
  | x::y::tl -> let a, b = split_list tl in x::a, y::b



(* Décalage de 1 bit vers les poids forts (multiplication par deux) *)
let shiftl1 n a = const "0" ++ (a % (0, n-2))

let nmulu n a b start_signal =
  let next_busy, set_next_busy = loop 1 in
  let busy = start_signal ^| (reg 1 next_busy) in

  (* 'mule' est intialisé à b au début de la multiplication,
      puis à chaque cycle est shifté de 1 bit vers la droite (donc perd le bit de poid faible) *)
  let mule, set_mule = loop n in
  let mule = set_mule (mux start_signal (((reg n mule) % (1, n-1)) ++ const "0") b) in
  (* 'adde' est initialisé à a étendu sur 32 bits au début de la multiplication,
      puis à chaque cycle est shifté de 1 bit vers la gauche (donc multiplié par 2) *)
  let adde, set_adde = loop (2*n) in
  let adde = set_adde (mux start_signal (shiftl1 (2*n) (reg (2*n) adde)) (a ++ (zeroes n))) in

  (* 'res' est un accumulateur qui contient le résultat que l'on calcule,
      il est initialisé à 0 au début de la multiplication, et à chaque cycle
      si mule[0] est non nul, on lui rajoute adde (c'est correct) *)
  let res, set_res = loop (2*n) in
  let t_res = mux start_signal (reg (2*n) res) (zeroes (2*n)) in
  let res = set_res (mux (mule ** 0) t_res (nadder (2*n) adde t_res)) in
  let work_remains = nonnull (n - 1) (mule % (1, n-1)) in

  let finished = 
    set_next_busy (busy ^& work_remains) ^.
    (not work_remains) ^& busy in

  res % (0, n-1), res % (n, 2*n-1), finished





let rec ndivu n a b start_signal = 

  let next_busy, set_next_busy = loop 1 in
  let busy = start_signal ^| (reg 1 next_busy) in

  let dd, set_dd = loop n in  (* dividende *)
  let q , set_q  = loop n in  (* quotient *)
  let r , set_r  = loop n in  (* reste *)

  (* L'algorithme doit se poursuivre sur n cycles
     exactement. c est une constante sur n bits, composée
     uniquement de 1, qui est décalée à chaque étape, si
     bien que le calcul est terminé quand c vaut 0 *)

  let c , set_c  = loop n in  
  let c = set_c (
    mux start_signal 
      (shiftl1 n (reg n c)) 
      ((const "0") ++ (rep (n-1) (const "1"))) ) in

  (* Initialisation de q et r *)
  let q  = mux start_signal (reg n q) (zeroes n) in
  let r  = mux start_signal (reg n r) (zeroes n) in

  (* A l'itération i (i = 0 ... n - 1) Le bit de poids fort de [dd]
     correspond au bit (n-i-1) du dividende original *)
  let dd = set_dd (mux start_signal (shiftl1 n (reg n dd)) a) in

  (* On abaisse le bit (n-i-1) du dividende *)
  let r = (dd ** (n-1)) ++ (r % (0, n-2)) in
  
  (* Si r >= d alors r := r - d et q(n-i-1) := 1 *)
  let rq = mux (ule_n n b r)
    (r               ++ ((const "0") ++ (q % (0, n-2))))
    ((nsubber n r b) ++ ((const "1") ++ (q % (0, n-2)))) in

  let r = set_r (rq % (0, n-1)) in
  let q = set_q (rq % (n, 2*n-1)) in

  let work_remains = nonnull n c in

  let finished = 
    set_next_busy (busy ^& work_remains) ^. 
      (not work_remains) ^& busy in

  dd ^. c ^.
    q, r, finished
  
                                   

(* zeroes (n-3) ++ const "110", zeroes (n-3) ++ const "110", 
   start_signal *)
(* TODO : unsigned division, returns quotient and remainder *)


let rec nmul n a b start_signal =
  zeroes (n-3) ++ const "101", zeroes (n-3) ++ const "101", start_signal
    (* TODO : signed multiplication ; returns low part and high part *)


let rec ndiv n a b start_signal =
    zeroes (n - 3) ++ const "011", zeroes (n - 3) ++ const "011", start_signal
    (* TODO : signed division *)


(* Shifts *)

let npshift_signed n p a b =
    a (* TODO (here b is a signed integer on p bits) *)

let op_lsl n a b =
    a (* TODO (b is unsigned, same size n) *)

let op_lsr n a b =
    a (* TODO (b is unsigned, same size n) *)

let op_asr n a b =
    a (* TODO (b unsigned size n) *)



(* Big pieces *)

let alu_comparer n f0 f a b =
    (*
        f0  f   action
        --  -   ------
        0   0   equal
        0   1   not equal
        0   2   equal
        0   3   not equal
        1   0   lt
        1   1   le
        1   2   lt unsigned
        1   3   le unsigned
    *)
    let eq_a_b = eq_n n a b in
    let eq_ne = mux (f ** 0) (eq_a_b) (not eq_a_b) in
    let lte_signed = mux (f ** 0) (lt_n n a b) (le_n n a b) in
    let lte_unsigned = mux (f ** 0) (ult_n n a b) (ule_n n a b) in
    let lte = mux (f ** 1) lte_signed lte_unsigned in
    mux f0 eq_ne lte

let alu_arith f0 f a b start_signal =
    (*  See table for ALU below *)
    let add = nadder 16 a b in
    let sub = nsubber 16 a b in
    let mul, mul2, mul_end_signal = nmul 16 a b start_signal in
    let div, div2, div_end_signal = ndiv 16 a b start_signal in
    let mulu, mulu2, mulu_end_signal = nmulu 16 a b start_signal in
    let divu, divu2, divu_end_signal = ndivu 16 a b start_signal in
    let q00 = mux (f ** 0) add sub in
    let q01 = mux (f ** 0) mul div in
    let q03 = mux (f ** 0) mulu divu in
    let q10 = mux (f ** 1) q00 q01 in
    let q11 = mux (f ** 1) q00 q03 in
    let q = mux f0 q10 q11 in
    let r01 = mux (f ** 0) mul2 div2 in
    let r03 = mux (f ** 0) mulu2 divu2 in
    let r10 = mux (f ** 1) (zeroes 16) r01 in
    let r11 = mux (f ** 1) (zeroes 16) r03 in
    let r = mux f0 r10 r11 in
    let s01 = mux (f ** 0) mul_end_signal div_end_signal in
    let s03 = mux (f ** 0) mulu_end_signal divu_end_signal in
    let s10 = mux (f ** 1) start_signal s01 in
    let s11 = mux (f ** 1) start_signal s03 in
    let end_signal = mux f0 s10 s11 in
    q, r, end_signal

let alu_logic f a b =
    (*  See table for ALU below *)
    let q0 = mux (f ** 0) (a ^| b) (a ^& b) in
    let q1 = mux (f ** 0) (a ^^ b) (not (a ^| b)) in
    mux (f ** 1) q0 q1

let alu_shifts f a b =
    (*  See table for ALU below *)
    let q1 = mux (f ** 0) (op_lsr 16 a b) (op_asr 16 a b) in
    mux (f ** 1) (op_lsl 16 a b) q1

let alu f1 f0 f a b start_signal =
    (*
        f1  f0  f   action
        --  --  -   ------
        0   0   0   add
        0   0   1   sub
        0   0   2   mul
        0   0   3   div
        0   1   0   addu
        0   1   1   subu
        0   1   2   mulu
        0   1   3   divu
        1   0   0   or
        1   0   1   and
        1   0   2   xor
        1   0   3   nor
        1   1   0   lsl
        1   1   1   lsl
        1   1   2   lsr
        1   1   3   asr
    *)
    let arith, arith_r, arith_end_signal = alu_arith f0 f a b start_signal in
    let logic = alu_logic f a b in
    let shifts = alu_shifts f a b in

    let q0 = mux f0 logic shifts in
    let s = mux f1 arith q0 in
    let r = mux f1 arith_r (zeroes 16) in
    let end_signal = mux f1 arith_end_signal start_signal in
    s, r, end_signal

