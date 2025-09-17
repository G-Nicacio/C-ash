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

## EBNF
```
PROGRAM        = HEADER?, { IMPORT }, { TOPLEVEL } ;
HEADER         = SECRETS, DATASOURCE? ;
SECRETS        = "secrets", STRING, ";" ;
DATASOURCE     = "datasource", "default", IDENT, STRING, ";" ;
IMPORT         = "import", IDENT, [ "as", IDENT ], ";" ;

TOPLEVEL       = BANK_DECL | RULE_DEF | FUNC_DECL | HANDLER | GLOBAL_DECL
               | OPEN_DECL | STMT ;

BANK_DECL      = "bank", IDENT, "{",
                   "currency", "=", CCY, ";",
                   { BANK_ITEM },
                 "}" ;
BANK_ITEM      = POLICY | CONNECT ;

POLICY         = "policy", IDENT, POLICY_BODY
               | "policy", "limit", ("daily" | "per_tx"), MONEY, ";" ;
POLICY_BODY    = "{", { POLICY_STMT }, "}" | ";" ;
POLICY_STMT    = IDENT, ";"
               | IDENT, IDENT, ";"
               | IDENT, NUMBER, ";"
               | IDENT, STRING, ";"
               | IDENT, MONEY, ";" ;

CONNECT        = "connect", IDENT, [ "using", "fx", "{", { FX_OPT }, "}" ], ";" ;
FX_OPT         = "spread", NUMBER, "bp", ";"
               | "max_per_day", MONEY, ";" ;

GLOBAL_DECL    = DECL ;
DECL           = "reserve", IDENT, ":", TYPE, [ "=", EXPR ], ";" ;
TYPE           = "int" | "money" | "bool" | "str" | BANKREF, ".", "account" ;
BANKREF        = IDENT ;

OPEN_DECL      = "open", IDENT, "(", ARG_LIST? , ")", "as", IDENT, ";" ;
ARG_LIST       = ARG, { ",", ARG } ;
ARG            = IDENT, "=", EXPR | EXPR ;

FUNC_DECL      = "fn", IDENT, "(", PARAMS? , ")", [ ":", TYPE ], BLOCK ;
PARAMS         = PARAM, { ",", PARAM } ;
PARAM          = IDENT, ":", TYPE ;

RULE_DEF       = "rule", IDENT, "(", PARAMS? , ")", ":", "int", BLOCK ;

HANDLER        = "on", EVENT, BLOCK ;
EVENT          = "card_insert" | "pin_ok" | "pin_fail" | "net_fail" | "idle" ;

BLOCK          = "{", { ITEM }, "}" ;
ITEM           = DECL | OPEN_DECL | STMT ;

STMT           = ASSIGN | IF | WHILE | FOR | TRANSACTION | CALL, ";" | RETURN | BLOCK ;

ASSIGN         = IDENT, "=", EXPR, ";"
               | "set", IDENT, "to", EXPR, ";" ;

IF             = "if", "(", EXPR, ")", BLOCK, [ "else", BLOCK ] ;
WHILE          = "while", "(", EXPR, ")", BLOCK ;
FOR            = "for", "(", ASSIGN, EXPR, ";", ASSIGN, ")", BLOCK ;

TRANSACTION    = "transaction", BLOCK, "onfail", BLOCK ;

RETURN         = "return", [ EXPR ], ";" ;

CALL           = IDENT, "(", ARG_LIST? , ")" ;

EXPR           = LOGIC_OR ;
LOGIC_OR       = LOGIC_AND, { "||", LOGIC_AND } ;
LOGIC_AND      = EQUALITY, { "&&", EQUALITY } ;
EQUALITY       = REL, { ("==" | "!="), REL } ;
REL            = TERM, { ("<" | ">" | "<=" | ">="), TERM } ;
TERM           = FACTOR, { ("+" | "-"), FACTOR } ;
FACTOR         = UNARY,  { ("*" | "/" | "%"), UNARY } ;
UNARY          = ("+" | "-"), UNARY | PRIMARY ;
PRIMARY        = NUMBER | BOOL | STRING | MONEY | IDENT | CALL | "(", EXPR, ")" ;

NUMBER         = DIGIT, { DIGIT } ;
DIGIT          = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
BOOL           = "true" | "false" ;
STRING         = "\"", { CHAR }, "\"" ;
CHAR           = ? qualquer char UTF-8 exceto " e \, com escapes \n \t \" \\ ? ;
IDENT          = LETTER, { LETTER | DIGIT | "_" } ;
LETTER         = "A"… "Z" | "a"… "z" | "_" ;
CCY            = "A"…"Z", "A"…"Z", "A"…"Z" ;            (* BRL, USD, EUR *)
MONEY          = NUMBER, WS?, "$", WS?, CCY ;            (* 100$BRL | 100 $ BRL *)

WS             = { " " | "\t" | "\r" | "\n" } ;
COMMENT        = "//", { ? not NL ? }, "\n"
               | "/*", { ? any ? }, "*/" ;
