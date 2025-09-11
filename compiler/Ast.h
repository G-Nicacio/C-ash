#ifndef CASH_AST_H
#define CASH_AST_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct { int val; char* ccy; } MoneyLit;

/* Lista genérica de nós ---------------------------------------------------- */
typedef struct AST AST;
typedef struct ASTList {
  AST** items;
  size_t count;
  size_t cap;
} ASTList;

/* Kinds da AST ------------------------------------------------------------- */
typedef enum {
  NK_PROGRAM,
  NK_BLOCK,

  NK_DECL,            /* reserve name: type = init */
  NK_OPEN,            /* open Bank(args) as var */

  NK_BANK,            /* bank name { currency=ccy; items... } */
  NK_POLICY_SIMPLE,   /* policy ident; */
  NK_POLICY_LIMIT,    /* policy limit daily/per_tx money */
  NK_POLICY_BODY,     /* policy ident { ... } */
  NK_CONNECT_SIMPLE,  /* connect Bank; */
  NK_CONNECT_FX,      /* connect Bank using fx { ... } */
  NK_FXOPT_SPREAD,    /* spread N bp */
  NK_FXOPT_MAXPDAY,   /* max_per_day money */

  NK_IF, NK_WHILE, NK_FOR,
  NK_TXN,             /* transaction block onfail block */

  NK_ASSIGN,          /* name = expr */
  NK_RETURN,
  NK_CALL,            /* name(args...) */

  NK_FN,              /* fn name(params)->rettype block */
  NK_PARAM,           /* name : type */
  NK_RULE,            /* rule name(params):int block */

  NK_ON,              /* on event block */

  /* Expressões / Literais */
  NK_NUM, NK_BOOL, NK_STR, NK_MONEY, NK_VAR,
  NK_UNOP, NK_BINOP, NK_CMP, NK_LOGIC,

  /* Tipos (representados como nodes simples de nome) */
  NK_TYPENAME,        /* "int","money","bool","str" */
  NK_BANK_ACCT_TYPE   /* Bank.account (bank name embutido) */
} ASTKind;

/* Nó genérico -------------------------------------------------------------- */
struct AST {
  ASTKind k;
  int line;

  union {
    /* PROGRAM/BLOCK */
    struct { ASTList* list; } list;

    /* DECL */
    struct { char* name; char* typestr; AST* init; } decl;

    /* OPEN */
    struct { char* bankName; ASTList* args; char* varName; } open_;

    /* BANK */
    struct { char* name; char* ccy; ASTList* items; } bank;

    /* POLICY */
    struct { char* ident; } policy_simple;
    struct { char* scope; MoneyLit money; } policy_limit;
    struct { char* ident; ASTList* body; } policy_body;

    /* CONNECT */
    struct { char* target; } conn_simple;
    struct { char* target; ASTList* fxopts; } conn_fx;

    /* FX options */
    struct { int bp; } fx_spread;
    struct { MoneyLit money; } fx_maxpday;

    /* IF/WHILE/FOR */
    struct { AST* cond; AST* thenB; AST* elseB; } if_;
    struct { AST* cond; AST* body; } while_;
    struct { AST* init; AST* cond; AST* step; AST* body; } for_;

    /* TXN */
    struct { AST* body; AST* onfail; } txn;

    /* ASSIGN */
    struct { char* name; AST* expr; } assign;

    /* RETURN */
    struct { AST* expr; } ret;

    /* CALL */
    struct { char* name; ASTList* args; } call;

    /* FN/PARAM/RULE */
    struct { char* name; ASTList* params; char* rettype; AST* body; } fn;
    struct { char* name; char* typestr; } param;
    struct { char* name; ASTList* params; AST* body; } rule;

    /* ON */
    struct { char* eventName; AST* body; } on;

    /* Literais / Vars / Ops */
    struct { int v; } num;
    struct { int v; } boolean;
    struct { char* s; } str;
    struct { MoneyLit m; } money;
    struct { char* name; } var;

    struct { int op; AST* rhs; } unop;
    struct { int op; AST* lhs; AST* rhs; } binop;
    struct { int op; AST* lhs; AST* rhs; } cmp;
    struct { int op; AST* lhs; AST* rhs; } logic;

    /* Tipos */
    struct { char* name; } tname;
    struct { char* bankName; } tbankacct;
  };
};

/* Criação e listas --------------------------------------------------------- */
AST*     ast_program_new(void);
void     ast_program_add(AST* program, AST* node);

AST*     ast_block_new(void);
void     ast_block_add(AST* block, AST* stmt);

ASTList* ast_list_new(void);
void     ast_list_add(ASTList* L, AST* node);

/* Construtores básicos ----------------------------------------------------- */
AST* ast_decl(const char* name, const char* typestr, AST* init);
AST* ast_open(const char* bankName, ASTList* args, const char* varName);

AST* ast_bank(const char* name, const char* ccy, ASTList* items);
AST* ast_policy_simple(const char* ident);
AST* ast_policy_limit(const char* scope, MoneyLit m);
AST* ast_policy_body(const char* ident, ASTList* body);

AST* ast_connect_simple(const char* target);
AST* ast_connect_fx(const char* target, ASTList* fx_opts);

AST* ast_fxopt_spread(int bp);
AST* ast_fxopt_max_per_day(MoneyLit m);

AST* ast_if(AST* cond, AST* thenB, AST* elseB);
AST* ast_while(AST* cond, AST* body);
AST* ast_for(AST* init, AST* cond, AST* step, AST* body);

AST* ast_txn(AST* body, AST* onfail);

AST* ast_assign(const char* name, AST* expr);
AST* ast_return(AST* expr);

AST* ast_call(const char* name, ASTList* args);

AST* ast_fn(const char* name, ASTList* params, const char* rettype, AST* body);
AST* ast_param(const char* name, const char* typestr);
AST* ast_rule(const char* name, ASTList* params, AST* body);

AST* ast_on(const char* eventName, AST* body);

/* Literais/expressões */
AST* ast_num(int v);
AST* ast_bool(int v);
AST* ast_str(const char* s);
AST* ast_money(MoneyLit m);
AST* ast_var(const char* name);

AST* ast_unop(int op, AST* rhs);
AST* ast_binop(int op, AST* lhs, AST* rhs);
AST* ast_cmp(int op, AST* lhs, AST* rhs);
AST* ast_logic(int op, AST* lhs, AST* rhs);

/* Tipos */
AST* ast_type_name(const char* tname);
AST* ast_bank_account_type(const char* bankName);

/* Debug opcional */
void ast_dump(AST* node, int indent);

/* Liberação (opcional implementar depois) */
void ast_free(AST* node);

#ifdef __cplusplus
}
#endif
#endif
