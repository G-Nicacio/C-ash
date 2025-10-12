# ===== top-level Makefile =====
CC := gcc
CFLAGS := -O2 -Wall

ROOT := $(CURDIR)
BUILD := $(ROOT)/build

# binários
CASHC := $(ROOT)/compiler/cashc
POCKETVM := $(BUILD)/pocketvm

# alvos "meta"
.PHONY: all compiler vm run-hello run EX= clean distclean

all: compiler vm

# compila o compilador (usa o Makefile que já existe em compiler/)
compiler:
	$(MAKE) -C compiler

# compila a VM (runner) em build/pocketvm
vm: $(POCKETVM)

$(POCKETVM): vm/pocketVM.c
	mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -o $(POCKETVM) vm/pocketVM.c

# gera examples/hello.cash -> build/hello.asm e executa na VM
run-hello: all
	mkdir -p $(BUILD)
	$(CASHC) examples/hello.cash $(BUILD)/hello.asm
	$(POCKETVM) $(BUILD)/hello.asm

# rodar um exemplo arbitrário:
# uso: make run EX=examples/dailyLimit.cash
run: all
	@if [ -z "$(EX)" ]; then echo "uso: make run EX=examples/algum.cash"; exit 2; fi
	mkdir -p $(BUILD)
	$(CASHC) $(EX) $(BUILD)/out.asm
	$(POCKETVM) $(BUILD)/out.asm

clean:
	$(MAKE) -C compiler clean
	rm -rf $(BUILD)

distclean: clean
	@echo "ok."
