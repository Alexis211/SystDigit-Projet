open Netlist_gen
open Alu

let zeroes n =
    const (String.make n '0')

let one n =
    const "1" ++ zeroes (n-1)
let two n =
    const "01" ++ zeroes (n-2)


let cpu_ram ra we wa d =
    (*  Ram chip has word size = 8 bits and address size = 16 bits
        0x0000 to 0x3FFF is ROM0
        0x4000 to 0x7FFF is unused, reserved for MMIO
        0x8000 to 0xFFFF is RAM *)
    let ra_hi1 = ra ** 15 in
    let ra_lo1 = ra % (0, 14) in
    let ra_hi2 = ra ** 14 in
    let ra_lo2 = ra % (0, 13) in
    let read_rom = (not ra_hi1) ^& (not ra_hi2) in
    let read_ram = ra_hi1 in
    let wa_hi1 = wa ** 15 in
    let wa_lo1 = wa % (0, 14) in
    let we_ram = we ^& wa_hi1 in
    
    let rd_rom = rom "ROM0" 14 8 ra_lo2 in
    let rd_ram = ram 15 8 ra_lo1 we_ram wa_lo1 d in
    mux read_ram (mux read_rom (zeroes 8) rd_rom) rd_ram

let r0 = zeroes 16
let r1, save_r1 = loop 16
let r2, save_r2 = loop 16
let r3, save_r3 = loop 16
let r4, save_r4 = loop 16
let r5, save_r5 = loop 16
let r6, save_r6 = loop 16
let r7, save_r7 = loop 16

let cpu_get_reg i =
    let a00 = mux (i ** 0) r0 r1 in
    let a01 = mux (i ** 0) r2 r3 in
    let a02 = mux (i ** 0) r4 r5 in
    let a03 = mux (i ** 0) r6 r7 in
    let a10 = mux (i ** 1) a00 a01 in
    let a11 = mux (i ** 1) a02 a03 in
    mux (i ** 2) a10 a11

let save_cpu_regs wr wd =
    let next_r1 = mux (eq_c 3 wr 1) r1 wd in
    let next_r2 = mux (eq_c 3 wr 2) r2 wd in
    let next_r3 = mux (eq_c 3 wr 3) r3 wd in
    let next_r4 = mux (eq_c 3 wr 4) r4 wd in
    let next_r5 = mux (eq_c 3 wr 5) r5 wd in
    let next_r6 = mux (eq_c 3 wr 6) r6 wd in
    let next_r7 = mux (eq_c 3 wr 7) r7 wd in

    save_r1 (reg 16 next_r1) ^.
    save_r2 (reg 16 next_r2) ^.
    save_r3 (reg 16 next_r3) ^.
    save_r4 (reg 16 next_r4) ^.
    save_r5 (reg 16 next_r5) ^.
    save_r6 (reg 16 next_r6) ^.
    save_r7 (reg 16 next_r7) ^.
    r0

(*
let ticker n =
    let k, save_k = loop n in
    let s = reg n k in
    let next = nadder_nocarry n s (one n) in
    ignore (save_k next) s

let tick1 = ticker 1
let tick2 = ticker 2
*)

let rl, rh, i, ex, exf, pc =
    let next_read, save_next_read = loop 1 in
    let read = not (reg 1 (not next_read)) in
    let next_pc, save_next_pc = loop 16 in
    let pc = reg 16 next_pc in

    let ra, we, wa, d = zeroes 16, zeroes 1, zeroes 16, zeroes 8 in
    let ram_read, save_ram_read = loop 8 in

    (* Read instruction low when read is set and instruction high on next tick *)
    let next_read_ihi, save_next_read_ihi = loop 1 in
    let read_ihi = reg 1 next_read_ihi in
    let read_ilow = read in

    let ra = mux read_ilow ra pc in
    let ilow = reg 8 (mux read_ilow (zeroes 8) ram_read) in
    let ra = mux read_ihi ra (nadder_nocarry 16 pc (one 16)) in
    let ihi = mux read_ihi (zeroes 8) ram_read in

    let read_ilow = save_next_read_ihi read_ilow in
    (* When execution has just been read, exec is true, and exec is false the rest of the time *)
    let exec = read_ihi in
    (* Keep same instruction in register until new instruction is read *)
    let si, save_i = loop 16 in
    let i = mux exec (reg 16 si) (ilow ++ ihi) in
    let i = save_i i in

    (* Execute instruction if exec is set *)
    let next_pc = nadder_nocarry 16 pc (two 16) in
    let exec_finished = exec in

    let i_i = i % (11, 15) in
    let i_r = i % (8, 10) in
    let i_ra = i % (5, 7) in
    let i_rb = i % (2, 4) in
    let i_f = i % (0, 1) in
    let i_id = i % (0, 7) in
    let i_jd = i % (0, 10) in
    let i_kd = i % (0, 4) in

    (* registers *)
    let v_r = cpu_get_reg i_r in
    let v_ra = cpu_get_reg i_ra in
    let v_rb = cpu_get_reg i_rb in
    let wr = zeroes 3 in
    let rwd = zeroes 16 in

    (* instruction : incri *)
    let instr_incri = exec ^& eq_c 5 i_i 0b00110 in
    let wr = mux instr_incri wr i_r in 
    let rwd = mux instr_incri rwd (nadder_nocarry 16 v_r (sign_extend 8 16 i_id)) in
    
    (* instruction : j *)
    let instr_j = exec ^& eq_c 5 i_i 0b01000 in
    let next_pc = mux instr_j next_pc (nadder_nocarry 16 pc (sign_extend 11 16 i_jd)) in
    (* instruction : jal *)
    let instr_jal = exec ^& eq_c 5 i_i 0b01001 in
    let next_pc = mux instr_jal next_pc (nadder_nocarry 16 pc (sign_extend 11 16 i_jd)) in
    let instr_jalxx = instr_jal in
    (* instruction : jr/jalr/jer/jner/jltr/jler/jltru/ljeru *)
    let instr_jxxr = exec ^& eq_c 4 (i_i % (1, 4)) 0b0101 in
    let f0 = i_i ** 0 in
    let instr_jr = (not f0) ^& (eq_c 2 i_f 0) in
    let instr_jalr = (not f0) ^& (eq_c 2 i_f 1) in
    let instr_jalxx = instr_jalxx ^| (instr_jxxr ^& instr_jalr) in
    let cond_jxxr = instr_jxxr ^& (alu_comparer 16 f0 i_f v_ra v_rb ^| instr_jr ^| instr_jalr) in
    let next_pc = mux cond_jxxr next_pc v_r in
    (* prologue for jal/jalr *)
    let wr = mux instr_jalxx wr (const "011") in
    let rwd = mux instr_jalxx rwd next_pc in

    (* instruction : lw/lwr/sw/swr *)
    let instr_lsw = eq_c 4 (i_i % (1, 4)) 0b1000 in
    let instr_lswr = eq_c 4 (i_i % (1, 4)) 0b1010 in
    let instr_lswx = instr_lsw ^| instr_lswr in
    let instr_swx = instr_lswx ^& (i_i ** 0) in
    let instr_lwx = instr_lswx ^& (not (i_i ** 0)) in

    let lswx_d = mux instr_lswr (sign_extend 5 16 i_kd) v_rb in
    let lswx_addr_lo = reg 16 (nadder_nocarry 16 v_ra lswx_d) in
    let lswx_addr_hi = let a, b = nadder 16 v_ra lswx_d (const "1") in b ^. reg 16 a in

    let lwx_load_lo = reg 1 (exec ^& instr_lwx) in
    let lwx_load_hi = reg 1 lwx_load_lo in
    let ra = mux lwx_load_lo ra lswx_addr_lo in
    let lwx_lo = reg 8 (mux lwx_load_lo (zeroes 8) ram_read) in
    let ra = mux lwx_load_hi ra lswx_addr_hi in
    let lwx_hi = mux lwx_load_hi (zeroes 8) ram_read in
    let wr = mux lwx_load_hi wr i_r in 
    let rwd = mux lwx_load_hi rwd (lwx_lo ++ lwx_hi) in
    let exec_finished = mux instr_lwx exec_finished lwx_load_hi in

    let swx_save_lo = reg 1 (exec ^& instr_swx) in
    let swx_save_hi = reg 1 swx_save_lo in
    let we = we ^| swx_save_lo in
    let wa = mux swx_save_lo wa lswx_addr_lo in
    let d = mux swx_save_lo d (v_r % (0, 7)) in
    let we = we ^| swx_save_hi in
    let wa = mux swx_save_hi wa lswx_addr_hi in
    let d = mux swx_save_hi d (v_r % (8, 15)) in
    let exec_finished = mux instr_lwx exec_finished swx_save_hi in


    save_cpu_regs wr rwd ^.
    save_ram_read (cpu_ram ra we wa d) ^.
    save_next_read exec_finished ^.
    save_next_pc (mux exec_finished pc next_pc) ^.
    read_ilow, read_ihi, i, exec, exec_finished, pc

let p =
    program
        []
        [
            "read_ilow", 1, rl;
            "read_ihi", 1, rh;
            "exec_instr", 1, ex;
            "exec_finished", 1, exf;
            "instruction", 16, i;
            "pc", 16, pc;
            "r0_Z", 16, r0;
            "r1_A", 16, r1;
            "r2_B", 16, r2;
            "r3_C", 16, r3;
            "r4_D", 16, r4;
            "r5_E", 16, r5;
            "r6_F", 16, r6;
            "r7_G", 16, r7;
        ]

let () = Netlist_gen.print stdout p

