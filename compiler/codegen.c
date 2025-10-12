/* =============================================================================
 * C-ash Codegen -> PocketASM
 * -----------------------------------------------------------------------------
 * - Emite assembly stack-based simplificado (PocketASM).
 * - Variáveis armazenadas por nome simbólico (STORE <name> / LOAD <name>).
 * - Expressões empilham valores; operações consomem pilha.
 * - Controle de fluxo com labels .L<n>.
 * - transaction { } onfail { } torna blocos e labels; rollback é delegada à VM.
 * - Chamada xfer(...) exige guarda enforce(...) no mesmo bloco (marca E001).
 * =============================================================================
 */

#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* ---------- Config de saída ------------------------------------------------ */

typedef struct GuardScope {
  int enforce_seen;            /* se vimos alguma guarda neste bloco */
  struct GuardScope* parent;
} GuardScope;

typedef struct {
  FILE* out;
  int   next_label;
  int   indent;

  int   in_transaction;
  int   tx_level;

  GuardScope* guard_scope;     /* pilha de flags por bloco */
} CG;

/* Helpers de escrita */
static void emit(CG* cg, const char* s) {
  for (int i=0;i<cg->indent;i++) fputc(' ', cg->out);
  fputs(s, cg->out);
  fputc('\n', cg->out);
}
static void emitf(CG* cg, const char* fmt, ...) {
  for (int i=0;i<cg->indent;i++) fputc(' ', cg->out);
  va_list ap; va_start(ap, fmt);
  vfprintf(cg->out, fmt, ap);
  va_end(ap);
  fputc('\n', cg->out);
}
static int new_label(CG* cg) { return cg->next_label++; }

/* Guard scope stack */
static void guard_push(CG* cg) {
  GuardScope* g = (GuardScope*)calloc(1,sizeof(GuardScope));
  g->enforce_seen = 0;
  g->parent = cg->guard_scope;
  cg->guard_scope = g;
}
static void guard_pop(CG* cg) {
  GuardScope* g = cg->guard_scope;
  if (g) {
    cg->guard_scope = g->parent;
    free(g);
  }
}
static void guard_mark_enforce(CG* cg) {
  if (cg->guard_scope) cg->guard_scope->enforce_seen = 1;
}
static int guard_seen(CG* cg) {
  GuardScope* g = cg->guard_scope;
  while (g) {
    if (g->enforce_seen) return 1;
    g = g->parent;
  }
  return 0;
}

/* ---------- Forward decls -------------------------------------------------- */
static void gen_node(CG* cg, AST* n);
static void gen_block(CG* cg, AST* b);
static void gen_expr(CG* cg, AST* e);
static void gen_stmt(CG* cg, AST* s);

/* ---------- Literais & variáveis ------------------------------------------ */
static void gen_push_money(CG* cg, MoneyLit m) {
  /* Representa money como inteiro 'm.val' + moeda 'm.ccy'.
     VM decide escala (Q24.8) e moeda. */
  emitf(cg, "PUSH_MONEY %d, %s", m.val, m.ccy);
}
static void gen_push_string(CG* cg, const char* s) {
  /* Emite string com aspas preservadas (escape simples). */
  /* Nota: para demo, imprimimos como está; um montador robusto deveria escapar. */
  emitf(cg, "PUSH_STR \"%s\"", s ? s : "");
}
static void gen_push_int(CG* cg, int v) {
  emitf(cg, "PUSH_I %d", v);
}
static void gen_load(CG* cg, const char* name) {
  emitf(cg, "LOAD %s", name);
}
static void gen_store(CG* cg, const char* name) {
  emitf(cg, "STORE %s", name);
}

/* ---------- Expressões ----------------------------------------------------- */
static void gen_binop(CG* cg, int op, AST* a, AST* b) {
  gen_expr(cg, a);
  gen_expr(cg, b);
  switch(op) {
    case '+': emit(cg, "ADD"); break;
    case '-': emit(cg, "SUB"); break;
    case '*': emit(cg, "MUL"); break;
    case '/': emit(cg, "DIV"); break;
    case '%': emit(cg, "MOD"); break;
    default:  emitf(cg, "; TODO BINOP %d", op); break;
  }
}
static void gen_cmp(CG* cg, int op, AST* a, AST* b) {
  gen_expr(cg, a);
  gen_expr(cg, b);
  switch(op) {
    case '<':    emit(cg, "CMP_LT"); break;
    case '>':    emit(cg, "CMP_GT"); break;
    case T_LE:   emit(cg, "CMP_LE"); break;
    case T_GE:   emit(cg, "CMP_GE"); break;
    case T_EQ:   emit(cg, "CMP_EQ"); break;
    case T_NE:   emit(cg, "CMP_NE"); break;
    default: emitf(cg, "; TODO CMP %d", op); break;
  }
}
static void gen_logic(CG* cg, int op, AST* a, AST* b) {
  /* Avaliação com short-circuit */
  int Lend = new_label(cg);
  int Lskip = new_label(cg);
  if (op == T_AND) {
    gen_expr(cg, a);                 /* empilha A */
    emit(cg, "DUP");
    emit(cg, "JZ .Lfalse_tmp");      /* se !A -> false */
    gen_expr(cg, b);                 /* empilha B */
    emit(cg, "AND");
    emitf(cg, "JMP .L%d", Lend);
    emit(cg, ".Lfalse_tmp:");
    emit(cg, "POP");
    emit(cg, "PUSH_I 0");
    emitf(cg, ".L%d:", Lend);
  } else if (op == T_OR) {
    gen_expr(cg, a);
    emit(cg, "DUP");
    emitf(cg, "JNZ .L%d", Lskip);    /* se A -> true */
    emit(cg, "POP");
    gen_expr(cg, b);
    emit(cg, "OR");
    emitf(cg, "JMP .L%d", Lend);
    emitf(cg, ".L%d:", Lskip);
    emit(cg, "PUSH_I 1");
    emitf(cg, ".L%d:", Lend);
  } else {
    /* fallback lógico simples (sem short-circuit) */
    gen_expr(cg, a);
    gen_expr(cg, b);
    emitf(cg, "; TODO LOGIC op=%d", op);
  }
}

static void gen_expr(CG* cg, AST* e) {
  if (!e) { emit(cg, "PUSH_I 0 ; <null-expr>"); return; }
  switch (e->k) {
    case NK_NUM:   gen_push_int(cg, e->num.v); break;
    case NK_BOOL:  gen_push_int(cg, e->boolean.v ? 1 : 0); break;
    case NK_STR:   gen_push_string(cg, e->str.s); break;
    case NK_MONEY: gen_push_money(cg, e->money.m); break;
    case NK_VAR:   gen_load(cg, e->var.name); break;

    case NK_UNOP:
      gen_expr(cg, e->unop.rhs);
      if (e->unop.op == '-') emit(cg, "NEG");
      /* '+' unário não faz nada */
      break;

    case NK_BINOP:
      gen_binop(cg, e->binop.op, e->binop.lhs, e->binop.rhs);
      break;

    case NK_CMP:
      gen_cmp(cg, e->cmp.op, e->cmp.lhs, e->cmp.rhs);
      break;

    case NK_LOGIC:
      gen_logic(cg, e->logic.op, e->logic.lhs, e->logic.rhs);
      break;

    case NK_CALL:
      /* call como expressão: empilha retorno */
      /* built-ins de exemplo: now(), region(), fx_rate(from,to) */
      if (strcmp(e->call.name,"now")==0) {
        emit(cg, "CALL now");
      } else if (strcmp(e->call.name,"region")==0) {
        emit(cg, "CALL region");
      } else if (strcmp(e->call.name,"fx_rate")==0) {
        /* args já vêm como parte do nó; aqui assumimos dois argumentos já empilhados */
        /* Estratégia: gerar os args e chamar */
        for (size_t i=0;i<e->call.args->count;i++) gen_expr(cg, e->call.args->items[i]);
        emit(cg, "CALL fx_rate  ; expects (from:str,to:str) -> money");
      } else {
        /* genérico: empilha args, chama */
        for (size_t i=0;i<e->call.args->count;i++) gen_expr(cg, e->call.args->items[i]);
        emitf(cg, "CALL %s", e->call.name);
      }
      break;

    default:
      emitf(cg, "PUSH_I 0 ; TODO expr kind=%d", e->k);
      break;
  }
}

/* ---------- Declarações/assign ------------------------------------------- */
static void gen_decl(CG* cg, AST* d) {
  /* Reserva símbolo por nome; se init existe, avalia e armazena. */
  emitf(cg, "; reserve %s : %s", d->decl.name, d->decl.typestr?d->decl.typestr:"(infer)");
  if (d->decl.init) {
    gen_expr(cg, d->decl.init);
    gen_store(cg, d->decl.name);
  } else {
    emit(cg, "PUSH_I 0");
    gen_store(cg, d->decl.name);
  }
}

static void gen_assign_stmt(CG* cg, AST* a) {
  gen_expr(cg, a->assign.expr);
  gen_store(cg, a->assign.name);
}

/* ---------- Controle de fluxo -------------------------------------------- */
static void gen_if(CG* cg, AST* n) {
  int Lelse = new_label(cg), Lend = new_label(cg);
  gen_expr(cg, n->if_.cond);
  emitf(cg, "JZ .L%d", Lelse);
  gen_node(cg, n->if_.thenB);
  emitf(cg, "JMP .L%d", Lend);
  emitf(cg, ".L%d:", Lelse);
  if (n->if_.elseB) gen_node(cg, n->if_.elseB);
  emitf(cg, ".L%d:", Lend);
}

static void gen_while(CG* cg, AST* n) {
  int Lbeg = new_label(cg), Lend = new_label(cg);
  emitf(cg, ".L%d:", Lbeg);
  gen_expr(cg, n->while_.cond);
  emitf(cg, "JZ .L%d", Lend);
  gen_node(cg, n->while_.body);
  emitf(cg, "JMP .L%d", Lbeg);
  emitf(cg, ".L%d:", Lend);
}

static void gen_for(CG* cg, AST* n) {
  int Lbeg = new_label(cg), Lend = new_label(cg);
  /* init */
  gen_stmt(cg, n->for_.init);
  emitf(cg, ".L%d:", Lbeg);
  /* cond */
  gen_expr(cg, n->for_.cond);
  emitf(cg, "JZ .L%d", Lend);
  /* body */
  gen_node(cg, n->for_.body);
  /* step */
  gen_stmt(cg, n->for_.step);
  emitf(cg, "JMP .L%d", Lbeg);
  emitf(cg, ".L%d:", Lend);
}

/* ---------- Transações ---------------------------------------------------- */
static void gen_txn(CG* cg, AST* n) {
  int Lfail = new_label(cg), Lend = new_label(cg);
  emit(cg, "; --- TRANSACTION BEGIN ---");
  emit(cg, "TX_BEGIN");
  int prev_tx = cg->in_transaction;
  cg->in_transaction = 1;
  cg->tx_level++;

  /* Cada transaction começa um escopo de guarda novo */
  guard_push(cg);

  /* corpo */
  gen_node(cg, n->txn.body);

  /* sucesso */
  emit(cg, "TX_COMMIT");
  emitf(cg, "JMP .L%d", Lend);

  /* falha */
  emitf(cg, ".L%d:", Lfail);
  emit(cg, "TX_ROLLBACK");
  gen_node(cg, n->txn.onfail);

  emitf(cg, ".L%d:", Lend);
  emit(cg, "; --- TRANSACTION END ---");

  /* fecha escopo de guarda da transação */
  guard_pop(cg);

  cg->tx_level--;
  cg->in_transaction = prev_tx;
}

/* ---------- Chamadas & built-ins ----------------------------------------- */

static int is_call_named(AST* c, const char* name) {
  return c && c->k==NK_CALL && c->call.name && strcmp(c->call.name,name)==0;
}

static void gen_call(CG* cg, AST* c) {
  /* Built-ins principais:
     - display(str)
     - print(str)
     - xfer(from, to, amount)   [requer enforce no bloco/txn]
     - beep(int)
     - approve/deny/cap/step_up/route_escrow/delay/notify  (antifraude)
     - open(...) as ... (já tratado fora)
  */

  /* Empilha argumentos em ordem */
  for (size_t i=0; c->call.args && i<c->call.args->count; i++) {
    gen_expr(cg, c->call.args->items[i]);
  }

  /* Marca guarda quando apropriado */
  if (is_call_named(c, "enforce")) {
    guard_mark_enforce(cg);
    emit(cg, "CALL enforce");
    return;
  }

  if (is_call_named(c, "display")) { emit(cg, "CALL display"); return; }
  if (is_call_named(c, "print"))   { emit(cg, "CALL print");   return; }
  if (is_call_named(c, "beep"))    { emit(cg, "CALL beep");    return; }

  if (is_call_named(c, "xfer")) {
    /* Regra E001: xfer precisa de enforce no escopo */
    if (!guard_seen(cg)) {
      emit(cg, "; E001: xfer sem guarda antifraude no escopo! (requer enforce(...))");
    }
    if (!cg->in_transaction) {
      emit(cg, "; E001b: xfer fora de transaction! (requer transaction {...} onfail {...})");
    }
    emit(cg, "CALL xfer");
    return;
  }

  /* Antifraude helpers (stubs de VM) */
  if (is_call_named(c, "approve"))       { emit(cg, "CALL approve"); return; }
  if (is_call_named(c, "deny"))          { emit(cg, "CALL deny"); return; }
  if (is_call_named(c, "cap"))           { emit(cg, "CALL cap"); return; }
  if (is_call_named(c, "step_up"))       { emit(cg, "CALL step_up"); return; }
  if (is_call_named(c, "route_escrow"))  { emit(cg, "CALL route_escrow"); return; }
  if (is_call_named(c, "delay"))         { emit(cg, "CALL delay"); return; }
  if (is_call_named(c, "notify"))        { emit(cg, "CALL notify"); return; }

  /* Sensores comuns (se vieram como statement) */
  if (is_call_named(c, "now"))       { emit(cg, "CALL now"); return; }
  if (is_call_named(c, "region"))    { emit(cg, "CALL region"); return; }
  if (is_call_named(c, "net_ok"))    { emit(cg, "CALL net_ok"); return; }
  if (is_call_named(c, "fx_rate"))   { emit(cg, "CALL fx_rate"); return; }

  /* Generics */
  emitf(cg, "CALL %s", c->call.name);
}

/* ---------- Blocos e statements ------------------------------------------ */

static void gen_stmt(CG* cg, AST* s) {
  if (!s) return;
  switch (s->k) {
    case NK_ASSIGN:     gen_assign_stmt(cg, s); break;
    case NK_IF:         gen_if(cg, s); break;
    case NK_WHILE:      gen_while(cg, s); break;
    case NK_FOR:        gen_for(cg, s); break;
    case NK_TXN:        gen_txn(cg, s); break;
    case NK_CALL:       gen_call(cg, s); break;
    case NK_RETURN:
      if (s->ret.expr) gen_expr(cg, s->ret.expr);
      emit(cg, "RET");
      break;
    case NK_BLOCK:
      gen_block(cg, s);
      break;

    /* No-ops (declarativos ou não-implementados no ASM) — comentários: */
    case NK_BANK:
      emitf(cg, "; BANK %s currency=%s", s->bank.name, s->bank.ccy); break;
    case NK_POLICY_SIMPLE:
      emitf(cg, "; POLICY %s;", s->policy_simple.ident); break;
    case NK_POLICY_LIMIT:
      emitf(cg, "; POLICY limit %s %d$ %s",
            s->policy_limit.scope, s->policy_limit.money.val, s->policy_limit.money.ccy); break;
    case NK_POLICY_BODY:
      emitf(cg, "; POLICY %s { ... }", s->policy_body.ident); break;
    case NK_CONNECT_SIMPLE:
      emitf(cg, "; CONNECT %s;", s->conn_simple.target); break;
    case NK_CONNECT_FX:
      emitf(cg, "; CONNECT %s using fx { ... }", s->conn_fx.target); break;
    case NK_RULE:
      emitf(cg, "; RULE %s { ... }", s->rule.name); break;
    case NK_ON:
      emitf(cg, "; ON %s { ... }", s->on.eventName); break;
    case NK_FN:
      emitf(cg, "; FN %s : %s  { ... }", s->fn.name, s->fn.rettype?s->fn.rettype:"void");
      /* (Opcional) poderia gerar label de função e corpo aqui. */
      break;

    case NK_DECL:
      gen_decl(cg, s);
      break;

    case NK_OPEN:
      /* open Bank(...) as var — delega ao runtime/VM para criar conta,
         mas registramos simbolicamente a variável. */
      emitf(cg, "; OPEN %s as %s", s->open_.bankName, s->open_.varName);
      /* avalia args (só para efeitos: VM pode consumir num CALL _open) */
      for (size_t i=0; s->open_.args && i<s->open_.args->count; i++) gen_expr(cg, s->open_.args->items[i]);
      emitf(cg, "CALL _open_%s", s->open_.bankName);
      gen_store(cg, s->open_.varName);
      break;

    default:
      emitf(cg, "; TODO stmt kind=%d", s->k);
      break;
  }
}

static void gen_block(CG* cg, AST* b) {
  guard_push(cg);          /* novo escopo de guarda */
  cg->indent += 2;
  for (size_t i=0; i<b->list.list->count; i++) {
    gen_stmt(cg, b->list.list->items[i]);
  }
  cg->indent -= 2;
  guard_pop(cg);
}

/* ---------- Toplevel / Programa ------------------------------------------ */

static void gen_node(CG* cg, AST* n) {
  if (!n) return;
  if (n->k == NK_BLOCK) { gen_block(cg, n); return; }
  if (n->k == NK_PROGRAM) {
    emit(cg, "; === PocketASM generated by C-ash ===");
    for (size_t i=0; i<n->list.list->count; i++) {
      gen_stmt(cg, n->list.list->items[i]);
    }
    emit(cg, "; === end ===");
    return;
  }
  /* fallback para nós isolados */
  gen_stmt(cg, n);
}

/* ---------- API pública --------------------------------------------------- */
int codegen_to_file(AST* program, const char* outpath) {
  CG cg = {0};
  cg.out = fopen(outpath, "w");
  if (!cg.out) {
    fprintf(stderr, "Erro: não consegui abrir '%s' para escrita.\n", outpath);
    return 1;
  }
  cg.next_label = 1;
  cg.indent = 0;
  cg.in_transaction = 0;
  cg.tx_level = 0;
  cg.guard_scope = NULL;

  /* gera */
  gen_node(&cg, program);

  fclose(cg.out);
  return 0;
}
