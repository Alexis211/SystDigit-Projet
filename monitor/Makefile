CF=main.c disp.c mon.c
HF=mon.h

mon: $(CF) $(HF)
	gcc -o $@ $(CF) -lcurses -O3

clean:
	rm mon
