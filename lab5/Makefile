CC=gcc
CFLAGS=-Wall -O2

vm.o : vm.c
	$(CC) $(CFLAGS) $<

vm_gc : vm_gc.c
	$(CC) $(CFLAGS) vm.o $<