/* =============================================================================
 * C-ash Parser (Bison)
 * -----------------------------------------------------------------------------
 * - Gramática alinhada com a EBNF enviada.
 * - Constrói AST usando funções ast_* (implementar em ast.c/ast.h).
 * - Precedência/associatividade: unário > * / % > + - > rel > == != > && > ||
 * - Eventos, bank/policy/connect, transaction, rule, on, open, reserve, etc.
 * =============================================================================
 */

%code requires {
  #include <stdio.h>
  #include <stdlib.h>

  /* ----------------------------- Money literal ----------------------------- */
  typedef struct { int val; char* ccy; } MoneyLit;

  /* ----------------------------- AST forward  ------------------------------ */
  /* Implemente estas structs/funções em ast.h / ast.c. Exemplos de nós:
     - ASTProgram, ASTBlock, ASTDecl, ASTOpen, ASTBank, ASTPolicy, ASTConnect
     - ASTIf, ASTWhile, ASTFor, ASTTxn, ASTAssign, ASTCall
     - ASTRule, ASTHandler, ASTFunc, ASTReturn
     - AST literals/vars/binops/comparisons/logicals
  */
  typedef struct AST AST;
  typedef struct ASTList ASTList;

  /* Criação/lista de nós (interface sugerida) */
  AST*     ast_program_new(void);
  void     ast_program_add(AST* program, AST* node);

  AST*     ast_block_new(void);
  void     ast_block_add(AST* block, AST* stmt);

  AST*     ast_decl(const char* name, const char* typestr, AST* init_expr);
  AST*     ast_open(const char* bankName, ASTList* args, const char* varName);

  AST*     ast_bank(const char* name, const char* ccy, ASTList* items);
  AST*     ast_policy_simple(const char* ident);
  AST*     ast_policy_limit(const char* scope /*daily|per_tx*/, MoneyLit m);
  AST*     ast_policy_body(const char* ident, ASTList* kvpairs); /* opcional */

  AST*     ast_connect_simple(const char* target);
  AST*     ast_connect_fx(const char* target, ASTList* fx_opts);

  AST*     ast_fxopt_spread(int bp);
  AST*     ast_fxopt_max_per_day(MoneyLit m);

  AST*     ast_if(AST* cond, AST* thenB, AST* elseB);
  AST*     ast_while(AST* cond, AST* body);
  AST*     ast_for(AST* init, AST* cond, AST* step, AST* body);

  AST*     ast_txn(AST* body, AST* onfail);

  AST*     ast_assign(const char* name, AST* expr);
  AST*     ast_return(AST* expr);

  AST*     ast_call(const char* name, ASTList* args);

  AST*     ast_fn(const char* name, ASTList* params, const char* rettype, AST* body);
  AST*     ast_param(const char* name, const char* typestr);

  AST*     ast_rule(const char* name, ASTList* params, AST* body);

  AST*     ast_on(const char* eventName, AST* body);

  /* Literais/expressões */
  AST*     ast_num(int v);
  AST*     ast_bool(int v);
  AST*     ast_str(const char* s);
  AST*     ast_money(MoneyLit m);
  AST*     ast_var(const char* name);

  AST*     ast_unop(int op, AST* rhs);
  AST*     ast_binop(int op, AST* lhs, AST* rhs);
  AST*     ast_cmp(int op, AST* lhs, AST* rhs);   /* < > <= >= == != */
  AST*     ast_logic(int op, AST* lhs, AST* rhs); /* && || */

  /* List helpers */
  ASTList* ast_list_new(void);
  void     ast_list_add(ASTList* L, AST* node);

  /* Type helpers */
  AST*     ast_type_name(const char* tname);   /* "int","money","bool","str","Bank.account" */
  AST*     ast_bank_account_type(const char* bankName);

  /* Error reporting from bison */
  void yyerror(const char* s);
}

/* Bison options */
%define api.pure full
%define api.value.type {union}
%define parse.error verbose
%locations

%code {
  extern int yylex(void* scanner, void* parser);
}

/* ------------------------------- %union ---------------------------------- */
%union {
  int       ival;
  char*     sval;
  MoneyLit  money;
  AST*      node;
  ASTList*  list;
}

/* ------------------------------- Tokens ---------------------------------- */
/* Palavras-chave & tipos */
%token T_RESERVE T_OPEN T_AS T_BANK T_CURRENCY T_POLICY T_CONNECT T_USING T_FX
%token T_TRANSACTION T_ONFAIL T_RULE T_ON
%token T_IF T_ELSE T_WHILE T_FOR T_SET T_TO T_FN T_RETURN
%token T_SECRETS T_DATASOURCE T_DEFAULT T_IMPORT
%token T_TINT T_TMONEY T_TBOOL T_TSTR

/* Eventos */
%token T_EV_CARD_INSERT T_EV_PIN_OK T_EV_PIN_FAIL T_EV_NET_FAIL T_EV_IDLE

/* Literais e idents */
%token T_NUMBER T_STRING T_IDENT T_MONEYLIT T_BOOL

/* Operadores compostos */
%token T_EQ T_NE T_LE T_GE T_AND T_OR

/* Erro do lexer */
%token T_ERROR

/* Símbolos de 1 char retornam os próprios: = : ; , ( ) { } < > + - * / % */

/* -------------------------- Precedência/Assoc ---------------------------- */
%right UPLUS UMINUS
%left  '*' '/' '%'
%left  '+' '-'
%left  '<' '>' T_LE T_GE
%left  T_EQ T_NE
%left  T_AND
%left  T_OR

/* ------------------------------ Tipagem ---------------------------------- */
%type  <node> program header secrets_decl datasource_decl import_decl toplevel
%type  <node> bank_decl bank_item policy_decl connect_decl policy_body
%type  <node> fx_option
%type  <node> global_decl decl type
%type  <node> open_decl call_arg
%type  <list> call_args call_args_opt
%type  <node> func_decl params param
%type  <list> params_opt
%type  <node> rule_def handler_decl
%type  <node> block block_item stmt assign if_stmt while_stmt for_stmt
%type  <node> transaction_stmt return_stmt
%type  <node> expr logic_or logic_and equality relation term factor unary primary
%type  <node> call
%type  <sval> event_name
%type  <money> money_lit
%type  <sval> bankref

%start program

%%

/* ============================== Programa ================================= */

program
  : header_opt import_list_opt toplevel_list_opt
    { $$ = ast_program_new(); /* add lists if you keep them separated */
      /* simplificação: ações de add acontecem nas regras *_list */
    }
  ;

header_opt
  : /* vazio */                     { $$ = NULL; }
  | header                          { $$ = $1; }
  ;

import_list_opt
  : /* vazio */                     { $$ = NULL; }
  | import_list                     { $$ = $1; }
  ;

toplevel_list_opt
  : /* vazio */                     { $$ = NULL; }
  | toplevel_list                   { $$ = $1; }
  ;

import_list
  : import_decl                     { $$ = ast_program_new(); ast_program_add($$, $1); }
  | import_list import_decl         { ast_program_add($1, $2); $$ = $1; }
  ;

toplevel_list
  : toplevel                        { $$ = ast_program_new(); ast_program_add($$, $1); }
  | toplevel_list toplevel          { ast_program_add($1, $2); $$ = $1; }
  ;

header
  : secrets_decl datasource_decl_opt { $$ = NULL; /* opcional: guardar no AST */ }
  ;

datasource_decl_opt
  : /* vazio */                     { $$ = NULL; }
  | datasource_decl                 { $$ = $1; }
  ;

secrets_decl
  : T_SECRETS T_STRING ';'          { $$ = NULL; /* armazenar caminho em AST se quiser */ }
  ;

datasource_decl
  : T_DATASOURCE T_DEFAULT T_IDENT T_STRING ';' { $$ = NULL; }
  ;

import_decl
  : T_IMPORT T_IDENT ';'                          { $$ = ast_call("import", NULL); /* stub */ }
  | T_IMPORT T_IDENT T_AS T_IDENT ';'             { $$ = ast_call("import_as", NULL); /* stub */ }
  ;

/* ================================ Bank =================================== */

toplevel
  : bank_decl
  | rule_def
  | func_decl
  | handler_decl
  | global_decl
  | open_decl
  | stmt
  ;

bank_decl
  : T_BANK T_IDENT '{'
      T_CURRENCY '=' T_IDENT ';'
      bank_items_opt
    '}'
    {
      $$ = ast_bank($2, $6, $8);
    }
  ;

bank_items_opt
  : /* vazio */                     { $$ = (AST*)ast_list_new(); }
  | bank_items                      { $$ = $1; }
  ;

bank_items
  : bank_item                       { $$ = (AST*)ast_list_new(); ast_list_add((ASTList*)$$, $1); }
  | bank_items bank_item            { ast_list_add((ASTList*)$1, $2); $$ = $1; }
  ;

bank_item
  : policy_decl                     { $$ = $1; }
  | connect_decl                    { $$ = $1; }
  ;

policy_decl
  : T_POLICY T_IDENT policy_body    { $$ = ast_policy_body($2, (ASTList*)$3); }
  | T_POLICY limit_kind money_lit ';'
    { $$ = ast_policy_limit($2, $3); }
  ;

limit_kind
  : /* tokenização por IDENT simplificada */ /* daily/per_tx como IDENT */
    /* Para garantir, tratamos como T_IDENT e validamos no ast_policy_limit */
    T_IDENT                         { $$ = $1; }
  ;

policy_body
  : '{' policy_body_items_opt '}'   { $$ = (AST*)$2; }
  | ';'                             { $$ = NULL; }
  ;

policy_body_items_opt
  : /* vazio */                     { $$ = ast_list_new(); }
  | policy_body_items               { $$ = $1; }
  ;

policy_body_items
  : policy_stmt                     { $$ = ast_list_new(); ast_list_add($$, $1); }
  | policy_body_items policy_stmt   { ast_list_add($1, $2); $$ = $1; }
  ;

policy_stmt
  : T_IDENT ';'                     { $$ = ast_call($1, NULL); }
  | T_IDENT T_IDENT ';'             { ASTList* L=ast_list_new(); ast_list_add(L, ast_var($2)); $$ = ast_call($1, L); }
  | T_IDENT T_NUMBER ';'            { ASTList* L=ast_list_new(); ast_list_add(L, ast_num($2)); $$ = ast_call($1, L); }
  | T_IDENT T_STRING ';'            { ASTList* L=ast_list_new(); ast_list_add(L, ast_str($2)); $$ = ast_call($1, L); }
  | T_IDENT money_lit ';'           { ASTList* L=ast_list_new(); ast_list_add(L, ast_money($2)); $$ = ast_call($1, L); }
  ;

connect_decl
  : T_CONNECT T_IDENT ';'
    { $$ = ast_connect_simple($2); }
  | T_CONNECT T_IDENT T_USING T_FX '{' fx_opts_opt '}' ';'
    { $$ = ast_connect_fx($2, $6); }
  ;

fx_opts_opt
  : /* vazio */                     { $$ = (AST*)ast_list_new(); }
  | fx_opts                         { $$ = $1; }
  ;

fx_opts
  : fx_option                       { $$ = (AST*)ast_list_new(); ast_list_add((ASTList*)$$, $1); }
  | fx_opts fx_option               { ast_list_add((ASTList*)$1, $2); $$ = $1; }
  ;

fx_option
  : /* spread 200bp; */
    T_IDENT T_NUMBER T_IDENT ';'
    {
      /* esperamos "spread" <num> "bp" */
      $$ = ast_fxopt_spread($2);
    }
  | /* max_per_day 1500$ BRL; */
    T_IDENT money_lit ';'
    {
      /* esperamos "max_per_day" <money> */
      $$ = ast_fxopt_max_per_day($2);
    }
  ;

/* =========================== Declarações Globais ========================= */

global_decl
  : decl                            { $$ = $1; }
  ;

decl
  : T_RESERVE T_IDENT ':' type '=' expr ';'
    {
      /* type para string textual: use ast_type_name no ast_decl */
      /* aqui passamos o nome do tipo no próprio nó type se preferir */
      $$ = ast_decl($2, NULL/*typestr in type node*/, $6);
      (void)$4; /* type node - trate dentro de ast_decl se for usar typed AST */
    }
  | T_RESERVE T_IDENT ':' type ';'
    {
      $$ = ast_decl($2, NULL, NULL);
      (void)$4;
    }
  ;

type
  : T_TINT                           { $$ = ast_type_name("int"); }
  | T_TMONEY                         { $$ = ast_type_name("money"); }
  | T_TBOOL                          { $$ = ast_type_name("bool"); }
  | T_TSTR                           { $$ = ast_type_name("str"); }
  | bankref '.' /*account*/          { $$ = ast_bank_account_type($1); }
  ;

bankref
  : T_IDENT                          { $$ = $1; }
  ;

/* ============================== open ... as ============================== */

open_decl
  : T_OPEN T_IDENT '(' call_args_opt ')' T_AS T_IDENT ';'
    { $$ = ast_open($2, (ASTList*)$4, $7); }
  ;

call_args_opt
  : /* vazio */                      { $$ = (ASTList*)ast_list_new(); }
  | call_args                        { $$ = $1; }
  ;

call_args
  : call_arg                         { $$ = (ASTList*)ast_list_new(); ast_list_add($$, $1); }
  | call_args ',' call_arg           { ast_list_add($1, $3); $$ = $1; }
  ;

call_arg
  : T_IDENT '=' expr                 { /* nomeado: guarda como call("=name",expr) ou par no AST */ $$ = ast_assign($1, $3); }
  | expr                             { $$ = $1; }
  ;

/* =============================== Funções ================================= */

func_decl
  : T_FN T_IDENT '(' params_opt ')' ret_type_opt block
    { $$ = ast_fn($2, (ASTList*)$4, (const char*)$6, $7); }
  ;

ret_type_opt
  : /* vazio */                      { $$ = (AST*)NULL; }
  | ':' type                         { /* se quiser string do tipo: ast_type_name-> guarda nome */ $$ = $2; }
  ;

params_opt
  : /* vazio */                      { $$ = (ASTList*)ast_list_new(); }
  | params                           { $$ = $1; }
  ;

params
  : param                            { $$ = (ASTList*)ast_list_new(); ast_list_add($$, $1); }
  | params ',' param                 { ast_list_add($1, $3); $$ = $1; }
  ;

param
  : T_IDENT ':' type                 { /* usa ast_type_name-> guarda name/type textual */ $$ = ast_param($1, NULL); (void)$3; }
  ;

/* ============================== Regras/On ================================ */

rule_def
  : T_RULE T_IDENT '(' params_opt ')' ':' T_TINT block
    { $$ = ast_rule($2, (ASTList*)$4, $8); }
  ;

handler_decl
  : T_ON event_name block
    { $$ = ast_on($2, $3); }
  ;

event_name
  : T_EV_CARD_INSERT                 { $$ = "card_insert"; }
  | T_EV_PIN_OK                      { $$ = "pin_ok"; }
  | T_EV_PIN_FAIL                    { $$ = "pin_fail"; }
  | T_EV_NET_FAIL                    { $$ = "net_fail"; }
  | T_EV_IDLE                        { $$ = "idle"; }
  ;

/* =============================== Blocos/Stmt ============================= */

block
  : '{' block_items_opt '}'          { $$ = $2; }
  ;

block_items_opt
  : /* vazio */                      { $$ = ast_block_new(); }
  | block_items                      { $$ = $1; }
  ;

block_items
  : block_item                       { $$ = ast_block_new(); ast_block_add($$, $1); }
  | block_items block_item           { ast_block_add($1, $2); $$ = $1; }
  ;

block_item
  : decl                             { $$ = $1; }
  | open_decl                        { $$ = $1; }
  | stmt                             { $$ = $1; }
  ;

stmt
  : assign                           { $$ = $1; }
  | if_stmt                          { $$ = $1; }
  | while_stmt                       { $$ = $1; }
  | for_stmt                         { $$ = $1; }
  | transaction_stmt                 { $$ = $1; }
  | call ';'                         { $$ = $1; }
  | return_stmt                      { $$ = $1; }
  | block                            { $$ = $1; }
  ;

assign
  : T_IDENT '=' expr ';'             { $$ = ast_assign($1, $3); }
  | T_SET T_IDENT T_TO expr ';'      { $$ = ast_assign($2, $4); }
  ;

if_stmt
  : T_IF '(' expr ')' block                              { $$ = ast_if($3, $5, NULL); }
  | T_IF '(' expr ')' block T_ELSE block                { $$ = ast_if($3, $5, $7); }
  ;

while_stmt
  : T_WHILE '(' expr ')' block                          { $$ = ast_while($3, $5); }
  ;

for_stmt
  : T_FOR '(' assign expr ';' assign ')' block          { $$ = ast_for($3, $4, $6, $8); }
  ;

transaction_stmt
  : T_TRANSACTION block T_ONFAIL block                  { $$ = ast_txn($2, $4); }
  ;

return_stmt
  : T_RETURN ';'                                        { $$ = ast_return(NULL); }
  | T_RETURN expr ';'                                   { $$ = ast_return($2); }
  ;

/* ============================== Expressões =============================== */

expr        : logic_or                                   { $$ = $1; } ;

logic_or    : logic_and                                  { $$ = $1; }
            | logic_or T_OR logic_and                    { $$ = ast_logic(T_OR, $1, $3); }
            ;

logic_and   : equality                                   { $$ = $1; }
            | logic_and T_AND equality                   { $$ = ast_logic(T_AND, $1, $3); }
            ;

equality    : relation                                   { $$ = $1; }
            | equality T_EQ relation                     { $$ = ast_cmp(T_EQ, $1, $3); }
            | equality T_NE relation                     { $$ = ast_cmp(T_NE, $1, $3); }
            ;

relation    : term                                       { $$ = $1; }
            | relation '<'  term                         { $$ = ast_cmp('<',  $1, $3); }
            | relation '>'  term                         { $$ = ast_cmp('>',  $1, $3); }
            | relation T_LE term                         { $$ = ast_cmp(T_LE, $1, $3); }
            | relation T_GE term                         { $$ = ast_cmp(T_GE, $1, $3); }
            ;

term        : factor                                     { $$ = $1; }
            | term '+' factor                            { $$ = ast_binop('+', $1, $3); }
            | term '-' factor                            { $$ = ast_binop('-', $1, $3); }
            ;

factor      : unary                                      { $$ = $1; }
            | factor '*' unary                           { $$ = ast_binop('*', $1, $3); }
            | factor '/' unary                           { $$ = ast_binop('/', $1, $3); }
            | factor '%' unary                           { $$ = ast_binop('%', $1, $3); }
            ;

unary       : primary                                    { $$ = $1; }
            | '+' unary %prec UPLUS                      { $$ = ast_unop('+', $2); }
            | '-' unary %prec UMINUS                     { $$ = ast_unop('-', $2); }
            ;

primary     : T_NUMBER                                   { $$ = ast_num($1); }
            | T_BOOL                                     { $$ = ast_bool($1); }
            | T_STRING                                   { $$ = ast_str($1); }
            | money_lit                                  { $$ = ast_money($1); }
            | T_IDENT                                    { $$ = ast_var($1); }
            | call                                       { $$ = $1; }
            | '(' expr ')'                               { $$ = $2; }
            ;

money_lit   : T_MONEYLIT                                 { $$ = $1; } ;

call
  : T_IDENT '(' call_args_opt ')'                        { $$ = ast_call($1, (ASTList*)$3); }
  ;

%%

/* ============================= User section ============================== */

void yyerror(const char* s) {
  fprintf(stderr, "Erro de sintaxe: %s\n", s);
}
