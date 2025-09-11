# C-ash
Uma linguagem de programação de domínio específico (DSL) para **sistemas bancários digitais, maquininhas e caixas eletrônicos virtuais**.

## Objetivo
- Minimizar riscos de erro humano em sistemas financeiros
- Tornar regras de negócio legíveis e auditáveis
- Garantir segurança por design com tipos monetários, transações atômicas e políticas antifraude

## Estrutura
- `compiler/`: compilador em C com Flex+Bison
- `vm/`: máquina virtual PocketATM-32
- `examples/`: programas de exemplo em `.cash`
- `docs/`: documentação, gramática e especificação

## Como rodar
```bash
cd compiler
make
./cashc ../examples/hello.cash -o hello.asm
../vm/pocket_vm hello.asm
