open Netlist_gen
open Alu

let ser_out, save_ser_out = loop 8
let ser_in_busy, save_ser_in_busy = loop 1

let dbg_ra, save_dbg_ra = loop 16
let dbg_read_data, save_dbg_read_data = loop 8
let dbg_wa, save_dbg_wa = loop 16
let dbg_write_data, save_dbg_write_data = loop 8

let d7_out = Array.to_list (Array.init 8 (fun i -> let v, sl = loop 8 in ("d7_"^(string_of_int i), v, sl, i)))

let cpu_ram ra we wa d =
    (*  Ram chip has word size = 8 bits and address size = 16 bits
        0x0000 to 0x3FFF is ROM0
        0x4000 to 0x7FFF is MMIO :
            byte 0x4000 is clock ticker (increments by one every tick ; zeroed on read)
            byte 0x4100 is serial input (zeroed on read)
            byte 0x4102 is serial output
        0x8000 to 0xFFFF is RAM *)
    let read_data = zeroes 8 in

    (* ROM chip *)
    let ra_hi1 = ra ** 15 in
    let ra_lo1 = ra % (0, 14) in
    let ra_hi2 = ra ** 14 in
    let ra_lo2 = ra % (0, 13) in
    let read_rom = (not ra_hi1) ^& (not ra_hi2) in
    let rd_rom = rom "ROM0" 14 8 ra_lo2 in
    let read_data = mux read_rom read_data rd_rom in

    (* RAM chip *)
    let read_ram = ra_hi1 in
    let wa_hi1 = wa ** 15 in
    let wa_lo1 = wa % (0, 14) in
    let we_ram = we ^& wa_hi1 in
    let rd_ram = ram 15 8 ra_lo1 we_ram wa_lo1 d in
    let read_data = mux read_ram read_data rd_ram in

    (* MMIO *)
    let read_tick_lo = eq_c 16 ra 0x4000 in
    let read_tick_hi = eq_c 16 ra 0x4001 in
    let next_tick, save_next_tick = loop 16 in
    let tick = nadder 16 (reg 16 next_tick) (get "tick" ++ zeroes 15) in
    let read_data =
        save_next_tick (mux read_tick_hi tick (zeroes 16)) ^.
        mux read_tick_lo 
            (mux read_tick_hi read_data (tick % (8, 15)))
            (tick % (0, 7)) in

    let write_ser = we ^& (eq_c 16 wa 0x4102) in
    let read_data =
        save_ser_out (mux write_ser (zeroes 8) d) ^.
        read_data in

    let read_ser = eq_c 16 ra 0x4100 in
    let next_ser, save_next_ser = loop 8 in
    let ser = reg 8 next_ser in
    let ser_in = get "ser_in" in
    let iser = nonnull 8 ser_in in
    let ser = mux iser ser ser_in in
    let ser_busy = nonnull 8 ser in
    let ser_busy = mux read_ser ser_busy (const "0") in
    let read_data =
        save_ser_in_busy ser_busy ^.
        save_next_ser (mux read_ser ser (zeroes 8)) ^.
        mux read_ser read_data ser in

    let read_data = List.fold_left
        (fun rd (_, prevd7digit, setd7digit, i) ->
            let save_that_digit = we ^& (eq_c 16 wa (0x4200 + i)) in
            setd7digit (mux save_that_digit (reg 8 prevd7digit) d) ^. rd)
        read_data d7_out in

    save_dbg_ra ra ^.
    save_dbg_read_data read_data ^.
    save_dbg_wa wa ^.
    save_dbg_write_data d ^.
    read_data
    

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
    let ra = mux read_ihi ra (nadder 16 pc (one 16)) in
    let ihi = mux read_ihi (zeroes 8) ram_read in

    let read_ilow = save_next_read_ihi read_ilow in
    (* When execution has just been read, exec is true, and exec is false the rest of the time *)
    let exec = read_ihi in
    (* Keep same instruction in register until new instruction is read *)
    let si, save_i = loop 16 in
    let i = mux exec (reg 16 si) (ilow ++ ihi) in
    let i = save_i i in

    (* Execute instruction if exec is set *)
    let next_pc = nadder 16 pc (two 16) in
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

    (* instruction : add/sub/mul/div/unsigned/or/and/xor/nor/lsl/lsr/asr *)
    let instr_alu = eq_c 3 (i_i % (2, 4)) 0b000 in
    let f1 = i_i ** 1 in
    let f0 = i_i ** 0 in
    let double_instr_alu = instr_alu ^& (not f1) ^& (i_f ** 1) ^& (ne_n 3 i_r (const "101")) in

    let alu_d1, alu_d2, instr_alu_finished = alu f1 f0 i_f v_ra v_rb (exec ^& instr_alu) in
    let instr_alu_store_2 = reg 1 (instr_alu_finished ^& double_instr_alu) in
    let wr = mux instr_alu_finished wr i_r in
    let rwd = mux instr_alu_finished rwd alu_d1 in
    let wr = mux instr_alu_store_2 wr (const "101") in
    let rwd = mux instr_alu_store_2 rwd (reg 16 alu_d2) in
    let exec_finished = mux instr_alu exec_finished 
        (mux double_instr_alu instr_alu_finished instr_alu_store_2) in


    (* instruction : se/sne/slt/slte/sleu/sleu *)
    let instr_sxxx = exec ^& (eq_c 4 (i_i % (1, 4)) 0b0010) in
    let f0 = i_i ** 0 in
    let cond_sxxx = alu_comparer 16 f0 i_f v_ra v_rb in
    let wr = mux instr_sxxx wr i_r in
    let rwd = mux instr_sxxx rwd (mux cond_sxxx (zeroes 16) (one 16)) in

    (* instruction : incri *)
    let instr_incri = exec ^& eq_c 5 i_i 0b00110 in
    let wr = mux instr_incri wr i_r in 
    let rwd = mux instr_incri rwd (nadder 16 v_r (sign_extend 8 16 i_id)) in
    (* instruction : shi *)
    let instr_shi = exec ^& eq_c 5 i_i 0b00111 in
    let wr = mux instr_shi wr i_r in
    let rwd = mux instr_shi rwd (npshift_signed 16 8 v_r i_id) in
    
    (* instruction : j *)
    let instr_j = exec ^& eq_c 5 i_i 0b01000 in
    let next_pc = mux instr_j next_pc (nadder 16 pc (sign_extend 11 16 i_jd)) in
    (* instruction : jal *)
    let link_pc = next_pc in
    let instr_jal = exec ^& eq_c 5 i_i 0b01001 in
    let next_pc = mux instr_jal next_pc (nadder 16 pc (sign_extend 11 16 i_jd)) in
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
    let rwd = mux instr_jalxx rwd link_pc in

    (* instruction : lra *)
    let instr_lra = exec ^& eq_c 5 i_i 0b01100 in
    let wr = mux instr_lra wr (const "101") in
    let rwd = mux instr_lra rwd (nadder 16 pc (sign_extend 11 16 i_jd)) in

    (* instruction : hlt *)
    let instr_hlt = exec ^& eq_c 5 i_i 0b01111 in
    let halted, set_halted = loop 1 in
    let halted = set_halted (instr_hlt ^| (reg 1 halted)) in
    let exec_finished = mux halted exec_finished (const "0") in

    (* instruction : lw/lwr/sw/swr *)
    let instr_lsw = eq_c 4 (i_i % (1, 4)) 0b1000 in
    let instr_lswr = eq_c 4 (i_i % (1, 4)) 0b1010 in
    let instr_lswx = instr_lsw ^| instr_lswr in
    let instr_swx = instr_lswx ^& (i_i ** 0) in
    let instr_lwx = instr_lswx ^& (not (i_i ** 0)) in

    let lswx_d = mux instr_lswr (sign_extend 5 16 i_kd) v_rb in
    let lswx_addr_lo = nadder 16 v_ra lswx_d in
    let lswx_addr_hi = let a, b = nadder_with_carry 16 v_ra lswx_d (const "1") in b ^. a in

    let lwx_load_lo = reg 1 (exec ^& instr_lwx) in
    let lwx_load_hi = reg 1 lwx_load_lo in
    let ra = mux lwx_load_lo ra (reg 16 lswx_addr_lo) in
    let lwx_lo = reg 8 (mux lwx_load_lo (zeroes 8) ram_read) in
    let ra = mux lwx_load_hi ra (reg 16 lswx_addr_hi) in
    let lwx_hi = mux lwx_load_hi (zeroes 8) ram_read in
    let wr = mux lwx_load_hi wr i_r in 
    let rwd = mux lwx_load_hi rwd (lwx_lo ++ lwx_hi) in
    let exec_finished = mux instr_lwx exec_finished lwx_load_hi in

    let swx_save_lo = exec ^& instr_swx in
    let swx_save_hi = reg 1 swx_save_lo in
    let we = we ^| swx_save_lo in
    let wa = mux swx_save_lo wa lswx_addr_lo in
    let d = mux swx_save_lo d (v_r % (0, 7)) in
    let we = we ^| swx_save_hi in
    let wa = mux swx_save_hi wa lswx_addr_hi in
    let d = mux swx_save_hi d (v_r % (8, 15)) in
    let exec_finished = mux instr_swx exec_finished swx_save_hi in

    (* instruction : lb/lbr/sb/sbr *)
    let instr_lsb = eq_c 4 (i_i % (1, 4)) 0b1001 in
    let instr_lsbr = eq_c 4 (i_i % (1, 4)) 0b1011 in
    let instr_lsbx = instr_lsb ^| instr_lsbr in
    let instr_sbx = instr_lsbx ^& (i_i ** 0) in
    let instr_lbx = instr_lsbx ^& (not (i_i ** 0)) in

    let lsbx_d = mux instr_lsbr (sign_extend 5 16 i_kd) v_rb in
    let lsbx_addr = nadder 16 v_ra lsbx_d in

    let lbx_load = reg 1 (exec ^& instr_lbx) in
    let ra = mux lbx_load ra (reg 16 lsbx_addr) in
    let wr = mux lbx_load wr i_r in
    let rwd = mux lbx_load rwd (ram_read ++ (zeroes 8)) in
    let exec_finished = mux instr_lbx exec_finished lbx_load in

    let sbx_save = exec ^& instr_sbx in
    let we = we ^| sbx_save in
    let wa = mux sbx_save wa lsbx_addr in
    let d = mux sbx_save d (v_r % (0, 7)) in
    (* no mux exec_finished, sb runs immediately *)

    (* instruction : lil/lilz/liu/liuz *)
    let instr_lixx = eq_c 3 (i_i % (2, 4))0b110 in
    let instr_lixz = i_i ** 0 in
    let instr_liux = i_i ** 1 in
    let wr = mux instr_lixx wr i_r in
    let rwd = mux instr_lixx rwd 
        (mux instr_liux
            ( (* lil *) i_id ++ (mux instr_lixz (v_r % (8, 15)) (zeroes 8)))
            ( (* liu *) (mux instr_lixz (v_r % (0, 7)) (zeroes 8)) ++ i_id)) in

    (* instruction : lie *)
    let instr_lie = eq_c 5 i_i 0b11100 in
    let wr = mux instr_lie wr i_r in
    let rwd= mux instr_lie rwd (sign_extend 8 16 i_id) in

    save_cpu_regs wr rwd ^.
    save_ram_read (cpu_ram ra we wa d) ^.
    save_next_read exec_finished ^.
    save_next_pc (mux exec_finished pc next_pc) ^.
    read_ilow, read_ihi, i, exec, exec_finished, pc

let p =
    program
        [
            "tick", 1;
            "ser_in", 8;
        ]
        (
            [
                "read_ilow", 1, rl;
                "read_ihi", 1, rh;
                "ex_instr", 1, ex;
                "ex_finish", 1, exf;
                "i", 16, i;
                "ra", 16, dbg_ra;
                "read_data", 8, dbg_read_data;
                "wa", 16, dbg_wa;
                "write_data", 8, dbg_write_data;
                "pc", 16, pc;
                "r0_Z", 16, r0;
                "r1_A", 16, r1;
                "r2_B", 16, r2;
                "r3_C", 16, r3;
                "r4_D", 16, r4;
                "r5_E", 16, r5;
                "r6_F", 16, r6;
                "r7_G", 16, r7;
                "ser_out", 8, ser_out;
                "ser_in_busy", 1, ser_in_busy;
            ] @ List.map (fun (name, out, _, _) -> (name, 8, out)) d7_out
        )

let () = Netlist_printer.print_program stdout p

