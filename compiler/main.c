// compiler/main.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"

/* Flex/Bison reentrantes (coadunado com %parse-param / %lex-param) */
int yylex_init(void** scanner);
int yylex_destroy(void* scanner);
void yyset_in(FILE* in_str, void* yyscanner);
int yyparse(void* scanner, AST** out_ast);

/* Codegen -> PocketASM */
int codegen_to_file(AST* program, const char* outpath);

static void usage(const char* prog) {
  fprintf(stderr,
    "C-ash compiler\n"
    "uso: %s <input.cash> <output.asm>\n"
    "ex.: %s examples/hello.cash build/hello.asm\n", prog, prog);
}

int main(int argc, char** argv) {
  if (argc != 3) { usage(argv[0]); return 2; }

  const char* inpath  = argv[1];
  const char* outpath = argv[2];

  /* Abre fonte .cash */
  FILE* fp = fopen(inpath, "rb");
  if (!fp) {
    perror("erro ao abrir input");
    fprintf(stderr, "arquivo: %s\n", inpath);
    return 2;
  }

  /* Inicializa scanner (Flex reentrante) e conecta ao arquivo */
  void* scanner = NULL;
  if (yylex_init(&scanner) != 0) {
    fprintf(stderr, "erro: yylex_init falhou\n");
    fclose(fp);
    return 2;
  }
  yyset_in(fp, scanner);

  /* Faz o parse e obtém AST raiz */
  AST* program = NULL;
  int pres = yyparse(scanner, &program);

  /* Libera scanner/arquivo */
  yylex_destroy(scanner);
  fclose(fp);

  if (pres != 0) {
    fprintf(stderr, "falha no parse (codigo=%d)\n", pres);
    return 3;
  }
  if (!program) {
    fprintf(stderr, "falha: AST vazia\n");
    return 3;
  }

  /* Gera assembly da VM alvo */
  if (codegen_to_file(program, outpath) != 0) {
    fprintf(stderr, "falha no codegen ao escrever '%s'\n", outpath);
    return 4;
  }

  /* Debug opcional da AST:
     ast_dump(program, 0);
     ast_free(program);
  */
  return 0;
}
