%{
  #include "parser.h"
  #include <string.h>
  #include <stdlib.h>
  #include <ctype.h>

  /* assinatura reentrante compatível com %locations do bison */
  #define YY_DECL int yylex(YYSTYPE* yylval_param, YYLTYPE* yylloc_param, void* yyscanner)

  /* NÃO redefinimos MoneyLit aqui: já vem de parser.h */

  /* Helpers reentrantes ------------------------------------------------------ */
  static char* cash_dup(const char* s, size_t n) {
    char* r = (char*)malloc(n+1);
    if (!r) { fprintf(stderr,"OOM\n"); exit(1); }
    memcpy(r,s,n); r[n]=0; return r;
  }

  /* Mapeia identificadores para palavras-chave (sem tocar yylval aqui) */
  static int keyword_or_ident(const char* id) {
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

    return T_IDENT;
  }

  /* Constrói yylval.money a partir de "NUM $ CCY" (reentrante) */
  static void fill_money_from_text(YYSTYPE* yylval_param,
                                  const char* text, size_t len, int lineno) {
    /* remove espaços */
    char* buf = (char*)malloc(len+1);
    if (!buf) { fprintf(stderr,"OOM\n"); exit(1); }
    size_t j=0;
    for (size_t i=0;i<len;i++) if (!isspace((unsigned char)text[i])) buf[j++]=text[i];
    buf[j]=0; /* esperado: DIGITS$CCC */

    char* p = strchr(buf,'$');
    if (!p || p==buf || !p[1]) {
      fprintf(stderr,"[linha %d] literal money inválido\n", lineno);
      free(buf); return;
    }
    *p = 0;
    const char* num = buf;
    const char* ccy = p+1;

    if (!(isupper((unsigned char)ccy[0]) && isupper((unsigned char)ccy[1]) &&
          isupper((unsigned char)ccy[2]) && ccy[3]=='\0')) {
      fprintf(stderr,"[linha %d] moeda deve ser ISO-4217 (ex.: BRL)\n", lineno);
      free(buf); return;
    }

    yylval_param->money.val = atoi(num);
    yylval_param->money.ccy = cash_dup(ccy, 3);
    free(buf);
  }

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

{WS}                  ;
{NL}                  ;

"//".*                         { /* single-line comment */ }
"/*"([^*]|\*+[^*/])*\*+"/"     { /* block comment */ }

\"{STRCHR}*\"         { yylval_param->sval = cash_dup(yytext+1, yyleng-2);
                        return T_STRING; }

{DIGIT}+              { yylval_param->ival = atoi(yytext); return T_NUMBER; }

{DIGIT}+[ \t]*\$[ \t]*{CCY} {
  fill_money_from_text(yylval_param, yytext, yyleng, yylineno);
  return T_MONEYLIT;
}

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

{ID}                  {
                        /* booleans literais */
                        if (strcmp(yytext,"true")==0)  { yylval_param->ival = 1; return T_BOOL; }
                        if (strcmp(yytext,"false")==0) { yylval_param->ival = 0; return T_BOOL; }

                        int tok = keyword_or_ident(yytext);
                        if (tok == T_IDENT) { yylval_param->sval = cash_dup(yytext, yyleng); }
                        return tok;
                      }

.                     {
                        fprintf(stderr,"[linha %d] caractere inesperado: '%s'\n", yylineno, yytext);
                        return T_ERROR;
                      }

%%

