#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* util -------------------------------------------------------------------- */
static char* dupstr(const char* s) {
  if (!s) return NULL;
  size_t n = strlen(s);
  char* r = (char*)malloc(n+1);
  memcpy(r, s, n+1);
  return r;
}

static AST* new_node(ASTKind k) {
  AST* n = (AST*)calloc(1, sizeof(AST));
  n->k = k;
  return n;
}

/* listas ------------------------------------------------------------------ */
ASTList* ast_list_new(void) {
  ASTList* L = (ASTList*)calloc(1, sizeof(ASTList));
  return L;
}

void ast_list_add(ASTList* L, AST* node) {
  if (!L) return;
  if (L->count+1 > L->cap) {
    size_t nc = (L->cap ? L->cap*2 : 4);
    L->items = (AST**)realloc(L->items, nc * sizeof(AST*));
    L->cap = nc;
  }
  L->items[L->count++] = node;
}

/* program/block ----------------------------------------------------------- */
AST* ast_program_new(void) {
  AST* n = new_node(NK_PROGRAM);
  n->list.list = ast_list_new();
  return n;
}

void ast_program_add(AST* program, AST* node) {
  if (!program || program->k != NK_PROGRAM) return;
  ast_list_add(program->list.list, node);
}

AST* ast_block_new(void) {
  AST* n = new_node(NK_BLOCK);
  n->list.list = ast_list_new();
  return n;
}

void ast_block_add(AST* block, AST* stmt) {
  if (!block || block->k != NK_BLOCK) return;
  ast_list_add(block->list.list, stmt);
}

/* decl/open --------------------------------------------------------------- */
AST* ast_decl(const char* name, const char* typestr, AST* init) {
  AST* n = new_node(NK_DECL);
  n->decl.name = dupstr(name);
  n->decl.typestr = typestr ? dupstr(typestr) : NULL;
  n->decl.init = init;
  return n;
}

AST* ast_open(const char* bankName, ASTList* args, const char* varName) {
  AST* n = new_node(NK_OPEN);
  n->open_.bankName = dupstr(bankName);
  n->open_.args = args;
  n->open_.varName = dupstr(varName);
  return n;
}

/* bank/policy/connect ----------------------------------------------------- */
AST* ast_bank(const char* name, const char* ccy, ASTList* items) {
  AST* n = new_node(NK_BANK);
  n->bank.name = dupstr(name);
  n->bank.ccy = dupstr(ccy);
  n->bank.items = items;
  return n;
}

AST* ast_policy_simple(const char* ident) {
  AST* n = new_node(NK_POLICY_SIMPLE);
  n->policy_simple.ident = dupstr(ident);
  return n;
}

AST* ast_policy_limit(const char* scope, MoneyLit m) {
  AST* n = new_node(NK_POLICY_LIMIT);
  n->policy_limit.scope = dupstr(scope);
  n->policy_limit.money = m;
  n->policy_limit.money.ccy = dupstr(m.ccy);
  return n;
}

AST* ast_policy_body(const char* ident, ASTList* body) {
  AST* n = new_node(NK_POLICY_BODY);
  n->policy_body.ident = dupstr(ident);
  n->policy_body.body = body;
  return n;
}

AST* ast_connect_simple(const char* target) {
  AST* n = new_node(NK_CONNECT_SIMPLE);
  n->conn_simple.target = dupstr(target);
  return n;
}

AST* ast_connect_fx(const char* target, ASTList* fx_opts) {
  AST* n = new_node(NK_CONNECT_FX);
  n->conn_fx.target = dupstr(target);
  n->conn_fx.fxopts = fx_opts;
  return n;
}

AST* ast_fxopt_spread(int bp) {
  AST* n = new_node(NK_FXOPT_SPREAD);
  n->fx_spread.bp = bp;
  return n;
}

AST* ast_fxopt_max_per_day(MoneyLit m) {
  AST* n = new_node(NK_FXOPT_MAXPDAY);
  n->fx_maxpday.money = m;
  n->fx_maxpday.money.ccy = dupstr(m.ccy);
  return n;
}

/* controle de fluxo ------------------------------------------------------- */
AST* ast_if(AST* cond, AST* thenB, AST* elseB) {
  AST* n = new_node(NK_IF);
  n->if_.cond = cond;
  n->if_.thenB = thenB;
  n->if_.elseB = elseB;
  return n;
}

AST* ast_while(AST* cond, AST* body) {
  AST* n = new_node(NK_WHILE);
  n->while_.cond = cond;
  n->while_.body = body;
  return n;
}

AST* ast_for(AST* init, AST* cond, AST* step, AST* body) {
  AST* n = new_node(NK_FOR);
  n->for_.init = init;
  n->for_.cond = cond;
  n->for_.step = step;
  n->for_.body = body;
  return n;
}

AST* ast_txn(AST* body, AST* onfail) {
  AST* n = new_node(NK_TXN);
  n->txn.body = body;
  n->txn.onfail = onfail;
  return n;
}

/* statements -------------------------------------------------------------- */
AST* ast_assign(const char* name, AST* expr) {
  AST* n = new_node(NK_ASSIGN);
  n->assign.name = dupstr(name);
  n->assign.expr = expr;
  return n;
}

AST* ast_return(AST* expr) {
  AST* n = new_node(NK_RETURN);
  n->ret.expr = expr;
  return n;
}

AST* ast_call(const char* name, ASTList* args) {
  AST* n = new_node(NK_CALL);
  n->call.name = dupstr(name);
  n->call.args = args ? args : ast_list_new();
  return n;
}

/* funções/regras/eventos -------------------------------------------------- */
AST* ast_fn(const char* name, ASTList* params, const char* rettype, AST* body) {
  AST* n = new_node(NK_FN);
  n->fn.name = dupstr(name);
  n->fn.params = params ? params : ast_list_new();
  n->fn.rettype = rettype ? dupstr(rettype) : NULL;
  n->fn.body = body;
  return n;
}

AST* ast_param(const char* name, const char* typestr) {
  AST* n = new_node(NK_PARAM);
  n->param.name = dupstr(name);
  n->param.typestr = typestr ? dupstr(typestr) : NULL;
  return n;
}

AST* ast_rule(const char* name, ASTList* params, AST* body) {
  AST* n = new_node(NK_RULE);
  n->rule.name = dupstr(name);
  n->rule.params = params ? params : ast_list_new();
  n->rule.body = body;
  return n;
}

AST* ast_on(const char* eventName, AST* body) {
  AST* n = new_node(NK_ON);
  n->on.eventName = dupstr(eventName);
  n->on.body = body;
  return n;
}

/* expressões/literais ----------------------------------------------------- */
AST* ast_num(int v)            { AST* n=new_node(NK_NUM);   n->num.v=v; return n; }
AST* ast_bool(int v)           { AST* n=new_node(NK_BOOL);  n->boolean.v=v; return n; }
AST* ast_str(const char* s)    { AST* n=new_node(NK_STR);   n->str.s=dupstr(s); return n; }
AST* ast_money(MoneyLit m)     { AST* n=new_node(NK_MONEY); n->money.m=m; n->money.m.ccy=dupstr(m.ccy); return n; }
AST* ast_var(const char* name) { AST* n=new_node(NK_VAR);   n->var.name=dupstr(name); return n; }

AST* ast_unop(int op, AST* rhs) {
  AST* n = new_node(NK_UNOP);
  n->unop.op = op;
  n->unop.rhs = rhs;
  return n;
}
AST* ast_binop(int op, AST* lhs, AST* rhs) {
  AST* n = new_node(NK_BINOP);
  n->binop.op = op;
  n->binop.lhs = lhs;
  n->binop.rhs = rhs;
  return n;
}
AST* ast_cmp(int op, AST* lhs, AST* rhs) {
  AST* n = new_node(NK_CMP);
  n->cmp.op = op;
  n->cmp.lhs = lhs;
  n->cmp.rhs = rhs;
  return n;
}
AST* ast_logic(int op, AST* lhs, AST* rhs) {
  AST* n = new_node(NK_LOGIC);
  n->logic.op = op;
  n->logic.lhs = lhs;
  n->logic.rhs = rhs;
  return n;
}

/* tipos ------------------------------------------------------------------- */
AST* ast_type_name(const char* tname) {
  AST* n = new_node(NK_TYPENAME);
  n->tname.name = dupstr(tname);
  return n;
}
AST* ast_bank_account_type(const char* bankName) {
  AST* n = new_node(NK_BANK_ACCT_TYPE);
  n->tbankacct.bankName = dupstr(bankName);
  return n;
}

/* debug printer ----------------------------------------------------------- */
static void indent(int n){ while(n--) fputc(' ', stderr); }

static void dump_list(ASTList* L, int ind) {
  if (!L) { indent(ind); fprintf(stderr, "(null-list)\n"); return; }
  for (size_t i=0;i<L->count;i++) {
    ast_dump(L->items[i], ind);
  }
}

void ast_dump(AST* n, int ind) {
  if (!n) { indent(ind); fprintf(stderr,"(null)\n"); return; }
  switch(n->k) {
    case NK_PROGRAM: indent(ind); fprintf(stderr,"PROGRAM\n"); dump_list(n->list.list, ind+2); break;
    case NK_BLOCK:   indent(ind); fprintf(stderr,"BLOCK\n");   dump_list(n->list.list, ind+2); break;

    case NK_DECL:
      indent(ind); fprintf(stderr,"DECL %s : %s\n", n->decl.name, n->decl.typestr?n->decl.typestr:"(infer)");
      if (n->decl.init) ast_dump(n->decl.init, ind+2);
      break;

    case NK_OPEN:
      indent(ind); fprintf(stderr,"OPEN bank=%s as %s\n", n->open_.bankName, n->open_.varName);
      dump_list(n->open_.args, ind+2);
      break;

    case NK_BANK:
      indent(ind); fprintf(stderr,"BANK %s currency=%s\n", n->bank.name, n->bank.ccy);
      dump_list(n->bank.items, ind+2);
      break;

    case NK_POLICY_SIMPLE:
      indent(ind); fprintf(stderr,"POLICY %s;\n", n->policy_simple.ident); break;
    case NK_POLICY_LIMIT:
      indent(ind); fprintf(stderr,"POLICY limit %s %d$ %s;\n",
        n->policy_limit.scope, n->policy_limit.money.val, n->policy_limit.money.ccy); break;
    case NK_POLICY_BODY:
      indent(ind); fprintf(stderr,"POLICY %s { ... }\n", n->policy_body.ident);
      dump_list(n->policy_body.body, ind+2); break;

    case NK_CONNECT_SIMPLE:
      indent(ind); fprintf(stderr,"CONNECT %s;\n", n->conn_simple.target); break;
    case NK_CONNECT_FX:
      indent(ind); fprintf(stderr,"CONNECT %s using fx { ... }\n", n->conn_fx.target);
      dump_list(n->conn_fx.fxopts, ind+2); break;

    case NK_FXOPT_SPREAD:
      indent(ind); fprintf(stderr,"spread %d bp\n", n->fx_spread.bp); break;
    case NK_FXOPT_MAXPDAY:
      indent(ind); fprintf(stderr,"max_per_day %d$ %s\n", n->fx_maxpday.money.val, n->fx_maxpday.money.ccy); break;

    case NK_IF:
      indent(ind); fprintf(stderr,"IF\n"); ast_dump(n->if_.cond, ind+2);
      indent(ind); fprintf(stderr,"THEN\n"); ast_dump(n->if_.thenB, ind+2);
      if (n->if_.elseB){ indent(ind); fprintf(stderr,"ELSE\n"); ast_dump(n->if_.elseB, ind+2); }
      break;
    case NK_WHILE:
      indent(ind); fprintf(stderr,"WHILE\n"); ast_dump(n->while_.cond, ind+2);
      ast_dump(n->while_.body, ind+2); break;
    case NK_FOR:
      indent(ind); fprintf(stderr,"FOR\n");
      ast_dump(n->for_.init, ind+2);
      ast_dump(n->for_.cond, ind+2);
      ast_dump(n->for_.step, ind+2);
      ast_dump(n->for_.body, ind+2); break;

    case NK_TXN:
      indent(ind); fprintf(stderr,"TRANSACTION\n");
      ast_dump(n->txn.body, ind+2);
      indent(ind); fprintf(stderr,"ONFAIL\n");
      ast_dump(n->txn.onfail, ind+2); break;

    case NK_ASSIGN:
      indent(ind); fprintf(stderr,"ASSIGN %s =\n", n->assign.name);
      ast_dump(n->assign.expr, ind+2); break;

    case NK_RETURN:
      indent(ind); fprintf(stderr,"RETURN\n");
      if (n->ret.expr) { ast_dump(n->ret.expr, ind+2); }
      break;


    case NK_CALL:
      indent(ind); fprintf(stderr,"CALL %s(...)\n", n->call.name);
      dump_list(n->call.args, ind+2); break;

    case NK_FN:
      indent(ind); fprintf(stderr,"FN %s : %s\n", n->fn.name, n->fn.rettype?n->fn.rettype:"void");
      dump_list(n->fn.params, ind+2);
      ast_dump(n->fn.body, ind+2); break;

    case NK_PARAM:
      indent(ind); fprintf(stderr,"PARAM %s : %s\n", n->param.name, n->param.typestr?n->param.typestr:"(infer)"); break;

    case NK_RULE:
      indent(ind); fprintf(stderr,"RULE %s\n", n->rule.name);
      dump_list(n->rule.params, ind+2);
      ast_dump(n->rule.body, ind+2); break;

    case NK_ON:
      indent(ind); fprintf(stderr,"ON %s\n", n->on.eventName);
      ast_dump(n->on.body, ind+2); break;

    case NK_NUM:
      indent(ind); fprintf(stderr,"NUM %d\n", n->num.v); break;
    case NK_BOOL:
      indent(ind); fprintf(stderr,"BOOL %s\n", n->boolean.v?"true":"false"); break;
    case NK_STR:
      indent(ind); fprintf(stderr,"STR \"%s\"\n", n->str.s?n->str.s:""); break;
    case NK_MONEY:
      indent(ind); fprintf(stderr,"MONEY %d$ %s\n", n->money.m.val, n->money.m.ccy); break;
    case NK_VAR:
      indent(ind); fprintf(stderr,"VAR %s\n", n->var.name); break;

    case NK_UNOP:
      indent(ind); fprintf(stderr,"UNOP %c\n", n->unop.op);
      ast_dump(n->unop.rhs, ind+2); break;
    case NK_BINOP:
      indent(ind); fprintf(stderr,"BINOP %c\n", n->binop.op);
      ast_dump(n->binop.lhs, ind+2);
      ast_dump(n->binop.rhs, ind+2); break;
    case NK_CMP:
      indent(ind); fprintf(stderr,"CMP %d\n", n->cmp.op);
      ast_dump(n->cmp.lhs, ind+2);
      ast_dump(n->cmp.rhs, ind+2); break;
    case NK_LOGIC:
      indent(ind); fprintf(stderr,"LOGIC %d\n", n->logic.op);
      ast_dump(n->logic.lhs, ind+2);
      ast_dump(n->logic.rhs, ind+2); break;

    case NK_TYPENAME:
      indent(ind); fprintf(stderr,"TYPE %s\n", n->tname.name); break;
    case NK_BANK_ACCT_TYPE:
      indent(ind); fprintf(stderr,"TYPE %s.account\n", n->tbankacct.bankName); break;
  }
}

/* free (placeholder – opcional expandir) ---------------------------------- */
static void free_list(ASTList* L) {
  if (!L) return;
  for (size_t i=0;i<L->count;i++) {
    // ast_free(L->items[i]); // implementar se quiser
  }
  free(L->items);
  free(L);
}

void ast_free(AST* n) {
  if (!n) return;
  switch(n->k) {
    case NK_PROGRAM:
    case NK_BLOCK:
      free_list(n->list.list); break;
    default: break;
  }
  free(n);
}
