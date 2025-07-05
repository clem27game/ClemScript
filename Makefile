CC = gcc
CFLAGS = -Wall -Wextra -std=c99
LDFLAGS = -lm

all: main

main: main.c
	$(CC) $(CFLAGS) -o main main.c $(LDFLAGS)

main-debug: main.c
	$(CC) $(CFLAGS) -g -o main-debug main.c $(LDFLAGS)

clean:
	rm -f main main-debug

.PHONY: all clean