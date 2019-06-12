all: trampoline2 check

check: escape_test trampoline_test

CC = gcc
CFLAGS = -std=c99 -O

trampoline2: trampoline.c
	$(CC) $(CFLAGS) -o $@ $<

trampoline_test: trampoline2
	bash trampoline_test.sh

escape_test:
	bash escape_test.sh
