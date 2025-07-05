all: main

CC = clang
override CFLAGS += -g -Wno-everything -pthread -lm

SRCS = $(shell find . -name '.ccls-cache' -type d -prune -o -type f -name '*.c' -print)
HEADERS = $(shell find . -name '.ccls-cache' -type d -prune -o -type f -name '*.h' -print)

main: $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) $(SRCS) -o "$@"

main-debug: $(SRCS) $(HEADERS)
	$(CC) $(CFLAGS) -O0 $(SRCS) -o "$@"

clean:
	rm -f main main-debug
CC = gcc
CFLAGS = -Wall -Wextra -std=c99
LDFLAGS = -lm

main: main.c
	$(CC) $(CFLAGS) -o main main.c $(LDFLAGS)

main-debug: main.c
	$(CC) $(CFLAGS) -g -o main-debug main.c $(LDFLAGS)

clean:
	rm -f main main-debug

.PHONY: clean
