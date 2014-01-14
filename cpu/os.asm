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
    jal ser_out_str

    li A 1994
    li B var_year
    sw A 0(B)
    li A 2
    li B var_month
    sb A 0(B)
    li A 26
    li B var_day
    sb A 0(B)

_main_loop:
    # Process serial input
    jal check_input
    jz A _end_process_input
    jal run_cmd
_end_process_input:

    # Process clock ticking
    li B _clock
    lw A 0(B)
    jz A _main_loop

    jal incr_clock
    jal disp_time
    j _main_loop

# PROCEDURE: incr_clock
# ROLE: take into account seconds increment
# ARGUMENTS: number of seconds to add, in register A
incr_clock:
    jz A _incr_clock_ret

    lb B var_sec
    add B B A
    li C 60
    divu A B C
    move B E
    sb B var_sec
    jz A _incr_clock_ret

    lb B var_min
    add B B A
    divu A B C
    move B E
    sb B var_min
    jz A _incr_clock_ret

    lb B var_hour
    add B B A
    li C 24
    divu A B C
    move B E
    sb B var_hour
    jz A _incr_clock_ret

    lb B var_day
    add B B A
    lb C var_month
    li D days_in_month
    add D D C
    lb C 0(D)
    divu A B C
    move B E
    sb B var_day
    jz A _incr_clock_ret

    lb B var_month
    add B B A
    li C 12
    divu A B C
    move B E
    sb B var_month
    jz A _incr_clock_ret

    lw B var_year
    add B B A
    sw B var_year


_incr_clock_ret:
    jr RA

# PROCEDURE: disp_clock
# ROLE: display current time
# ARGUMENTS: none
disp_time:
    push RA

    lw A var_year
    jal ser_out_int_dec
    li A '-'
    li E _output
    sb A 0(E)
    
    lb A var_month
    incri A 1
    jal ser_out_int_dec_two_digit
    li A '-'
    li E _output
    sb A 0(E)

    lb A var_day
    incri A 1
    jal ser_out_int_dec_two_digit
    li A ' '
    li E _output
    sb A 0(E)

    lb A var_hour
    jal ser_out_int_dec_two_digit
    li A ':'
    li E _output
    sb A 0(E)

    lb A var_min
    jal ser_out_int_dec_two_digit
    li A ':'
    li E _output
    sb A 0(E)
     
    lb A var_sec
    jal ser_out_int_dec_two_digit
    li A '\n'
    li E _output
    sb A 0(E)

    pop RA
    jr RA


# PROCEDURE: run_cmd
# ROLE: execute and clear command stored in cmdline
# ARGUMENTS: none
run_cmd:
    push RA

    li A prompt
    jal ser_out_str
    li A cmdline
    jal ser_out_str
    li A endl
    jal ser_out_str
    
    li A error
    jal ser_out_str

    li A cmdline_used
    sw Z 0(A)

    pop RA
    jr RA
    

# PROCEDURE: ser_out_int_dec
# ROLE: write a decimal integer to serial output
# ARGUMENTS: integer to write in register A
ser_out_int_dec:
    jz A _display_zero
    push Z
    li B 10
_divide_int_by_ten:
    jz A _show_dec
    divu A A B
    move C E
    addi C C '0'
    push C
    j _divide_int_by_ten
_show_dec:
    li C _output
    pop A
    jz A _return_out_dec
    sb A 0(C)
    j _show_dec
_return_out_dec:
    jr RA
_display_zero:
    li C _output
    li D '0'
    sb D 0(C)
    jr RA

# PROCEDURE: ser_out_two_digit
# ROLE: write only the two lower digits of a decimal integer to serial output
# ARGUMENTS: integer to write in register A
ser_out_int_dec_two_digit:
    li B 10
    divu A A B
    move C E
    divu A A B
    move D E
    li B _output
    addi D D '0'
    addi C C '0'
    sw D 0(B)
    sw C 0(B)
    jr RA


# PROCEDURE: ser_out_str
# ROLE: write null-terminated string to serial output
# ARGUMENTS: address of string in register A
ser_out_str:
    li C _output
_ser_out_str_loop:
    lb B 0(A)
    jz B _ser_out_str_ret
    sb B 0(C)
    incri A 1
    j _ser_out_str_loop
_ser_out_str_ret:
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
    jal ser_out_str

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
    jal ser_out_str
    pop D
    jalr D
    li A testfail
    jz B _unit_test_failed
    li A testok
_unit_test_failed:
    jal ser_out_str

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

    li C 61
    li D 5
    divu C C D
    move D E
    sei A C 12
    and B B A
    sei A D 1
    and B B A

    li C 54200
    li D 4203
    divu C C D
    move D E
    sei A C 12
    and B B A
    sei A D 3764
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


days_in_month:
    byte 31 28 31 30 31 30 31 31 30 31 30 31


.data
# Space where command-line is buffered from serial input (256 bytes)
cmdline:
    byte 256
# Number of bytes used in the command-line buffer (1 word)
cmdline_used:
    word 1

var_sec:
    byte 1
var_min:
    byte 1
var_hour:
    byte 1
var_day:
    byte 1
var_month:
    byte 1
var_year:
    word 1




