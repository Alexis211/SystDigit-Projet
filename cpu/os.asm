# CONVENTION:
#       return value for functions : in register A
#       arguments for functions : registers A, B, C, D
#       all registers are caller-saved, except SP which is preserved by function calls

.text
    jal run_unit_tests 

    li A msghello
    jal ser_out_msg

    push Z
main_loop:
    # Process serial input
    jal check_input
    jz A end_process_input
    jal run_cmd
end_process_input:

    # Process clock ticking
    pop D
    li B _clock
    lw B 0(B)
    add D D B
    push D

    jz B main_loop
    li A msgtick
    jal ser_out_msg
    j main_loop

# PROCEDURE: run_cmd
# ROLE: execute and clear command stored in cmdline
# ARGUMENTS: none
run_cmd:
    push RA

    li A prompt
    jal ser_out_msg
    li A cmdline
    jal ser_out_msg
    li A endl
    jal ser_out_msg
    
    li A error
    jal ser_out_msg

    li A cmdline_used
    sw Z 0(A)

    pop RA
    jr RA
    

# PROCEDURE: ser_out_msg
# ROLE: write null-terminated string to serial output
# ARGUMENTS: address of string in register A
ser_out_msg:
    li C _output
ser_out_msg_loop:
    lb B 0(A)
    jz B ser_out_msg_ret
    sb B 0(C)
    incri A 1
    j ser_out_msg_loop
ser_out_msg_ret:
    jr RA

# PROCEDURE: check_input
# ROLE: check if an input byte is available. if it is, and is different from '\n' (10), add it to cmdline
# ARGUMENTS: none
# RETURN VALUE: 1 if read byte was '\n', 0 otherwise
# WARNING: no buffer overflow check.
check_input:
    li A _input
    lb A 0(A)
    jz A check_input_ret
    move B A
    sei A A '\n'
    jz A add_b_to_string
    move B Z
add_b_to_string:
    push A
    li A cmdline
    li D cmdline_used
    lw C 0(D)
    add A A C
    sb B 0(A)
    incri C 1
    sw C 0(D)
    pop A
    jz A check_input
check_input_ret:
    jr RA

# PROCEDURE: run_unit_tests
# ROLE: check that CPU features work correctly ; displays message to serial output
# ARGUMENTS: none
run_unit_tests:
  push RA
  
  li A testbegin
  jal ser_out_msg

  li A test0
  jal ser_out_msg
  jal unit_test_0
  li A testfail
  jz B t0fail
  li A testok
t0fail:
  jal ser_out_msg

  li A test1
  jal ser_out_msg
  jal unit_test_1
  li A testfail
  jz B t1fail
  li A testok
t1fail:
  jal ser_out_msg

  li A test2
  jal ser_out_msg
  jal unit_test_2
  li A testfail
  jz B t2fail
  li A testok
t2fail:
  jal ser_out_msg

  li A test3
  jal ser_out_msg
  jal unit_test_3
  li A testfail
  jz B t2fail
  li A testok
t3fail:
  jal ser_out_msg

  pop RA
  jr RA

unit_test_0:
  li B 1

  li C 12
  li D 44
  add C C D
  sei A C 56
  and B B A

  li C -7
  li D 7
  add C C D
  se A C Z
  and B B A

  jr RA
unit_test_1:
  li B 1
  jr RA
unit_test_2:
  li B 1
  jr RA
unit_test_3:
  li B 1
  jr RA

    

# READ-ONLY PROGRAM DATA
msghello:
    ascii "Hello, world!\n"
msgtick:
    ascii " ..."
prompt:
    ascii "\n$ "
endl:
    ascii "\n"
error:
    ascii "Sorry but I'm to stupid to understand that.\n"

testbegin:
  ascii "Runing CPU unit tests...\n"
testok:
  ascii "OK\n"
testfail:
  ascii "FAIL\n"
test0:
  ascii "Addition/substraction: "
test1:
  ascii "Unsigned multiplication: "
test2:
  ascii "Unsigned division: "
test3:
  ascii "Signed division/multiplication: "



.data
# Space where command-line is buffered from serial input
cmdline:
    byte 256
# Number of bytes used in the command-line buffer
cmdline_used:
    word 1





