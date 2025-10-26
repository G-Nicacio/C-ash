# ===== top-level Makefile =====
CC := gcc
CFLAGS := -O2 -Wall

ROOT := $(CURDIR)
BUILD := $(ROOT)/build

# binários
CASHC := $(ROOT)/compiler/cashc
POCKETVM := python3 $(ROOT)/vm/pocketVM.py

# alvos "meta"
.PHONY: all compiler vm run-hello run run-example clean distclean

all: compiler

# compila o compilador (usa o Makefile que já existe em compiler/)
compiler:
	@$(MAKE) -s -C compiler

# o alvo "vm" agora é simbólico (nada pra compilar)
vm:
	@echo "(usando VM em Python)"

# gera examples/hello.cash -> build/hello.asm e executa na VM
run-hello: all
	@mkdir -p $(BUILD)
	@$(CASHC) examples/hello.cash $(BUILD)/hello.asm
	@echo "---- executando hello ----"
	@$(POCKETVM) $(BUILD)/hello.asm

# rodar um exemplo arbitrário:
# uso: make run EX=examples/dailyLimit.cash
run: all
	@if [ -z "$(EX)" ]; then echo "uso: make run EX=examples/algum.cash"; exit 2; fi
	@mkdir -p $(BUILD)
	@$(CASHC) $(EX) $(BUILD)/out.asm
	@echo "---- executando $(EX) ----"
	@$(POCKETVM) $(BUILD)/out.asm

# rodar um exemplo por nome sem path (ex: make run-example EX=txn_fail)
run-example:
	@$(MAKE) -s -C compiler
	@mkdir -p $(BUILD)
	@$(CASHC) examples/$(EX).cash $(BUILD)/$(EX).asm
	@echo "---- executando $(EX) ----"
	@$(POCKETVM) $(BUILD)/$(EX).asm

clean:
	@$(MAKE) -s -C compiler clean
	@rm -rf $(BUILD)
	@echo "limpo."

distclean: clean
	@echo "ok."
