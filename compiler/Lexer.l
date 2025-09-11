%{
  #include "parser.h"
  #include <string.h>
  #include <stdlib.h>
  #include <ctype.h>

  /* Garante typedef de MoneyLit se não vier do parser.h */
  #ifndef CASH_LEX_MONEYLIT_DEF
  #define CASH_LEX_MONEYLIT_DEF
  typedef struct { int val; char* ccy; } MoneyLit;
  #endif

  /* Helpers ----------------------------------------------------------------- */
  static char* cash_dup(const char* s, size_t n) {
    char* r = (char*)malloc(n+1);
    if (!r) { fprintf(stderr,"OOM\n"); exit(1); }
    memcpy(r,s,n); r[n]=0; return r;
  }

  /* Mapeia identificadores para palavras-chave */
  static int keyword_or_ident(const char* id) {
    /* keywords da linguagem */
    if (!strcmp(id,"reserve"))     return T_RESERVE;
    if (!strcmp(id,"open"))        return T_OPEN;
    if (!strcmp(id,"as"))          return T_AS;
    if (!strcmp(id,"bank"))        return T_BANK;
    if (!strcmp(id,"currency"))    return T_CURRENCY;
    if (!strcmp(id,"policy"))      return T_POLICY;
    if (!strcmp(id,"connect"))     return T_CONNECT;
    if (!strcmp(id,"using"))       return T_USING;
    if (!strcmp(id,"fx"))          return T_FX;
    if (!strcmp(id,"transaction")) return T_TRANSACTION;
    if (!strcmp(id,"onfail"))      return T_ONFAIL;
    if (!strcmp(id,"rule"))        return T_RULE;
    if (!strcmp(id,"on"))          return T_ON;
    if (!strcmp(id,"if"))          return T_IF;
    if (!strcmp(id,"else"))        return T_ELSE;
    if (!strcmp(id,"while"))       return T_WHILE;
    if (!strcmp(id,"for"))         return T_FOR;
    if (!strcmp(id,"set"))         return T_SET;
    if (!strcmp(id,"to"))          return T_TO;
    if (!strcmp(id,"fn"))          return T_FN;
    if (!strcmp(id,"return"))      return T_RETURN;
    if (!strcmp(id,"secrets"))     return T_SECRETS;
    if (!strcmp(id,"datasource"))  return T_DATASOURCE;
    if (!strcmp(id,"default"))     return T_DEFAULT;
    if (!strcmp(id,"import"))      return T_IMPORT;

    /* tipos */
    if (!strcmp(id,"int"))         return T_TINT;
    if (!strcmp(id,"money"))       return T_TMONEY;
    if (!strcmp(id,"bool"))        return T_TBOOL;
    if (!strcmp(id,"str"))         return T_TSTR;

    /* eventos */
    if (!strcmp(id,"card_insert")) return T_EV_CARD_INSERT;
    if (!strcmp(id,"pin_ok"))      return T_EV_PIN_OK;
    if (!strcmp(id,"pin_fail"))    return T_EV_PIN_FAIL;
    if (!strcmp(id,"net_fail"))    return T_EV_NET_FAIL;
    if (!strcmp(id,"idle"))        return T_EV_IDLE;

    /* booleanos literais */
    if (!strcmp(id,"true"))  { yylval.ival = 1; return T_BOOL; }
    if (!strcmp(id,"false")) { yylval.ival = 0; return T_BOOL; }

    return T_IDENT;
  }

  /* Constrói yylval.money a partir de "NUM $ CCY" com espaços opcionais */
  static void fill_money_from_yytext() {
    /* Remove espaços: copia para buffer sem espaço */
    const char* t = yytext;
    size_t n = yyleng;
    char* buf = (char*)malloc(n+1);
    size_t j=0;
    for (size_t i=0;i<n;i++) if (!isspace((unsigned char)t[i])) buf[j++]=t[i];
    buf[j]=0; /* agora formato esperado: DIGITS$CCC */

    /* Split no '$' */
    char* p = strchr(buf,'$');
    if (!p || p==buf || !p[1]) { free(buf); yyerror("literal money inválido"); return; }
    *p = 0;
    const char* num = buf;
    const char* ccy = p+1;

    /* valida moeda [A-Z]{3} */
    if (!(isupper((unsigned char)ccy[0]) && isupper((unsigned char)ccy[1]) && isupper((unsigned char)ccy[2]) && ccy[3]=='\0')) {
      free(buf); yyerror("moeda deve ser ISO-4217 (ex.: BRL)"); return;
    }

    yylval.money.val = atoi(num);
    yylval.money.ccy = cash_dup(ccy, 3);
    free(buf);
  }

  int yywrap(void){ return 1; }
%}

%option noinput nounput nodefault noyywrap yylineno
%option reentrant bison-bridge
%option warn
%option yymore

/* ============================ DEFINIÇÕES REGEX ============================ */
WS      [ \t\r]+
NL      \n
DIGIT   [0-9]
ID      [A-Za-z_][A-Za-z0-9_]*
CCY     [A-Z]{3}
ESC     \\([0nt"\\]|x[0-9A-Fa-f]{2})
STRCHR  ([^"\\]|{ESC})

%%

/* ----------------------- Espaços e comentários --------------------------- */
{WS}                  /* ignora */;
{NL}                  /* conta linha via %option yylineno */ ;

"//".*                /* ignora comentário de linha */;
"/*"([^*]|(\*+[^*/]))*\*+ "/"    /* ignora comentário de bloco */;

/* ----------------------- Literais básicos ------------------------------- */
\"{STRCHR}*\"         { /* STRING */
                        /* remove aspas e mantém escapes como vieram */
                        yylval.sval = cash_dup(yytext+1, yyleng-2);
                        return T_STRING;
                      }

{DIGIT}+              { yylval.ival = atoi(yytext); return T_NUMBER; }

/* ----------------------- Literal monetário ------------------------------ */
/* Formas aceitas:  100$BRL  | 100$ BRL | 100 $ BRL */
{DIGIT}+{WS}*"$"{WS}*{CCY} { fill_money_from_yytext(); return T_MONEYLIT; }

/* ----------------------- Operadores e símbolos -------------------------- */
"=="                  { return T_EQ; }
"!="                  { return T_NE; }
"<="                  { return T_LE; }
">="                  { return T_GE; }
"&&"                  { return T_AND; }
"||"                  { return T_OR; }

"="                   { return '='; }
":"                   { return ':'; }
";"                   { return ';'; }
","                   { return ','; }
"("                   { return '('; }
")"                   { return ')'; }
"{"                   { return '{'; }
"}"                   { return '}'; }
"<"                   { return '<'; }
">"                   { return '>'; }
"+"                   { return '+'; }
"-"                   { return '-'; }
"*"                   { return '*'; }
"/"                   { return '/'; }
"%"                   { return '%'; }

/* ----------------------- Identificadores / Keywords --------------------- */
{ID}                  {
                        int tok = keyword_or_ident(yytext);
                        if (tok == T_IDENT) { yylval.sval = cash_dup(yytext, yyleng); }
                        return tok;
                      }

/* ----------------------- Erros léxicos ---------------------------------- */
.                     {
                        fprintf(stderr,"[linha %d] caractere inesperado: '%s'\n", yylineno, yytext);
                        return T_ERROR;
                      }

%%

/* Fim do lexer.l */
