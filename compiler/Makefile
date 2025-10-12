CC=gcc
CFLAGS=-O2 -Wall

all: cashc

parser.c parser.h: parser.y
	@bison -d -v parser.y -o parser.c

lexer.c: lexer.l parser.h
	@flex --header-file=lexer.h -o lexer.c lexer.l

cashc: parser.o lexer.o ast.o codegen.o main.o
	@$(CC) $(CFLAGS) -o cashc parser.o lexer.o ast.o codegen.o main.o

%.o: %.c
	@$(CC) $(CFLAGS) -c -o $@ $<

clean:
	@rm -f *.o parser.c parser.h parser.output lexer.c lexer.h cashc
