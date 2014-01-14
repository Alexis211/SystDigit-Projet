# CONVENTION:
#       Return value for functions : in register A
#       Arguments for functions : registers A, B, C, D
#       All registers are caller-saved, except SP which is preserved by function calls
#       Labels beginning with '_' are not meant to be used outside of the function where
#           they are declared.

.text

# PROCEDURE: main loop
# ROLE: starts with the CPU then runs continuously
    jal run_unit_tests 

    li A msghello
    jal ser_out_msg

    push Z
_main_loop:
    # Process serial input
    jal check_input
    jz A _end_process_input
    jal run_cmd
_end_process_input:

    # Process clock ticking
    pop D
    li B _clock
    lw B 0(B)
    add D D B
    push D

    jz B _main_loop
    li A msgtick
    jal ser_out_msg
    j _main_loop

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
_ser_out_msg_loop:
    lb B 0(A)
    jz B _ser_out_msg_ret
    sb B 0(C)
    incri A 1
    j _ser_out_msg_loop
_ser_out_msg_ret:
    jr RA

# PROCEDURE: check_input
# ROLE: check if an input byte is available. if it is, and is different from '\n' (10), add it to cmdline
# ARGUMENTS: none
# RETURN VALUE: 1 if read byte was '\n', 0 otherwise
# WARNING: no buffer overflow check.
check_input:
    li A _input
    lb A 0(A)
    jz A _check_input_ret
    move B A
    sei A A '\n'
    jz A _ci_add_b_to_string
    move B Z
_ci_add_b_to_string:
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
_check_input_ret:
    jr RA

# PROCEDURE: run_unit_tests
# ROLE: check that CPU features work correctly ; displays message to serial output
# ARGUMENTS: none
run_unit_tests:
    push RA

    li A testbegin
    jal ser_out_msg

    move B Z

_unit_test_begin:
    li D testlist
    add D D B
    lw D 0(D)
    jz D _unit_tests_done
    
    push B
    push D
    li A teststr
    add A A B
    lw A 0(A)
    jal ser_out_msg
    pop D
    jalr D
    li A testfail
    jz B _unit_test_failed
    li A testok
_unit_test_failed:
    jal ser_out_msg

    pop B
    addi B B 2
    j _unit_test_begin

_unit_tests_done:
    pop RA
    jr RA


unit_test_0:    # Addition / substraction
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

    li C 32767
    li D 32767
    add C C D
    li D 2
    sub D Z D
    se A C D
    and B B A

    jr RA

unit_test_1:  # Unsigned multiplication
    li B 1

    li C 12
    li D 44
    mulu C C D
    move D E
    sei A C 528
    and B B A
    se A D Z
    and B B A

    li C 744
    li D 1244
    mulu C C D
    move D E
    sei A C 8032
    and B B A
    sei A D 14
    and B B A
    
    jr RA

unit_test_2:        # Unsigned division
    li B 1

    li C 60
    li D 5
    divu C C D
    move D E
    sei A C 12
    and B B A
    se A D Z
    and B B A

    li C 14129
    li D 477
    divu C C D
    move D E
    sei A C 31
    and B B A
    sei A D 272
    and B B A

    jr RA
unit_test_3:        # Signed multiplication/division
    li B 0
    jr RA

    

# READ-ONLY PROGRAM DATA

# General strings
msghello:
    ascii "\nHello, world!\n"
msgtick:
    ascii " ..."
prompt:
    ascii "\n$ "
endl:
    ascii "\n"
error:
    ascii "Sorry but I'm to stupid to understand that.\n"

# For unit-tests
testlist:
    word unit_test_0 unit_test_1 unit_test_2 unit_test_3 0
teststr:
    word test0 test1 test2 test3 0
testbegin:
    ascii "Runing CPU unit tests...\n"
testok:
    ascii "OK\n"
testfail:
    ascii "FAIL\n"
test0:
    ascii "Addition/substraction.......... "
test1:
    ascii "Unsigned multiplication........ "
test2:
    ascii "Unsigned division.............. "
test3:
    ascii "Signed multiplication/division. "



.data
# Space where command-line is buffered from serial input (256 bytes)
cmdline:
    byte 256
# Number of bytes used in the command-line buffer (1 word)
cmdline_used:
    word 1





