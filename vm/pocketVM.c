// pocketVM.c — Intérprete simples do PocketASM gerado pelo C-ash
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* --------------------- Tipos básicos --------------------- */
typedef struct { int val; char *ccy; } Money;
typedef enum { V_INT, V_STR, V_ACCOUNT, V_MONEY } VKind;

typedef struct {
  VKind k;
  int   own;          // 1 = dono (libera), 0 = não-owning
  union {
    int    i;
    char*  s;
    Money  m;
  } v;
} Value;

typedef struct { Value *data; size_t n, cap; } Stack;
typedef struct { char *name; Value v; } Var;
typedef struct { Var *it; size_t n, cap; } Env;

typedef struct { char *label; int pc; } Label;
typedef struct { char **line; int n; Label *lab; int nlab; } Program;

/* --------------------- Helpers de string/valores --------------------- */
static char* dupstr(const char* s){
  if(!s) return NULL;
  size_t n = strlen(s);
  char* r = malloc(n+1);
  memcpy(r,s,n+1);
  return r;
}

static Value v_copy(const Value* a){
  Value r = *a;
  if (a->k==V_STR || a->k==V_ACCOUNT){
    r.v.s = a->v.s ? dupstr(a->v.s) : NULL;
    r.own = 1;
  } else if (a->k==V_MONEY){
    r.v.m.ccy = a->v.m.ccy ? dupstr(a->v.m.ccy) : NULL;
    r.own = 1;
  } else {
    r.own = 0;
  }
  return r;
}

static void v_free(Value* v){
  if(!v) return;
  if (v->own){
    if (v->k==V_STR || v->k==V_ACCOUNT){
      free(v->v.s);
    } else if (v->k==V_MONEY){
      free(v->v.m.ccy);
    }
  }
  v->own = 0;
}

/* --- parse de "POLICY limit ..." vindo do ASM --- */
/* Aceita: "POLICY limit daily 1000$BRL" ou "POLICY limit daily 1000$ BRL" */
static int parse_limit_from_text(const char* s, int* out_val, char out_ccy[4]) {
  // pula até "POLICY limit"
  const char* p = strstr(s, "POLICY limit");
  if (!p) return 0;

  // depois de "POLICY limit " vem o scope (daily/per_tx)
  // então um número, '$', opcional espaço, e 3 letras
  int val = 0;
  char ccy[8] = "";

  // tenta com e sem espaço após '$'
  if (sscanf(p, "POLICY limit %*s %d$ %3s", &val, ccy) == 2 ||
      sscanf(p, "POLICY limit %*s %d$%3s", &val, ccy) == 2) {
    if (out_val) *out_val = val;
    if (out_ccy) { out_ccy[0]=ccy[0]; out_ccy[1]=ccy[1]; out_ccy[2]=ccy[2]; out_ccy[3]=0; }
    return 1;
  }
  return 0;
}

/* --------------------- Stack --------------------- */
static void push(Stack *S, Value v){
  if(S->n==S->cap){
    S->cap = S->cap ? S->cap*2 : 16;
    S->data = realloc(S->data, S->cap*sizeof(Value));
  }
  S->data[S->n++] = v;
}
static Value pop(Stack *S){
  if(!S->n){ fprintf(stderr,"stack underflow\n"); exit(1); }
  return S->data[--S->n];
}
static Value top(Stack *S){
  if(!S->n){ fprintf(stderr,"stack empty\n"); exit(1); }
  return S->data[S->n-1];
}

/* --------------------- Ambiente --------------------- */
static Var* env_find(Env* E, const char* name){
  for(size_t i=0;i<E->n;i++)
    if(strcmp(E->it[i].name,name)==0) return &E->it[i];
  return NULL;
}

static Value env_get(Env* E, const char* name){
  Var* v = env_find(E,name);
  if(!v){
    fprintf(stderr,"[env] undefined var '%s'\n",name);
    Value z={.k=V_INT,.v.i=0};
    return z;
  }
  return v_copy(&v->v);
}

static void env_set(Env* E, const char* name, Value v){
  Var* ex = env_find(E,name);
  if(ex){
    v_free(&ex->v);
    ex->v = v;
    return;
  }
  if(E->n==E->cap){
    E->cap = E->cap ? E->cap*2 : 8;
    E->it = realloc(E->it, E->cap*sizeof(Var));
  }
  E->it[E->n].name = dupstr(name);
  E->it[E->n].v = v;
  E->n++;
}

static void env_clear(Env* E){
  for(size_t i=0;i<E->n;i++){
    free(E->it[i].name);
    v_free(&E->it[i].v);
  }
  free(E->it);
  E->it=NULL; E->n=E->cap=0;
}

/* --------------------- Snapshots --------------------- */
typedef struct Snap {
  Env env;
  struct Snap* prev;
} Snap;

static Snap* tx_push(Snap* top, Env* E){
  Snap* s = calloc(1,sizeof(Snap));
  s->prev = top;
  for(size_t i=0;i<E->n;i++){
    Var* v = &E->it[i];
    Value nv = v_copy(&v->v);
    env_set(&s->env, v->name, nv);
  }
  return s;
}

static void tx_pop_commit(Snap** top){
  Snap* s=*top; if(!s) return;
  env_clear(&s->env);
  *top=s->prev;
  free(s);
}

static void tx_pop_rollback(Snap** top, Env* E){
  Snap* s=*top; if(!s) return;
  env_clear(E);
  *E = s->env;
  *top = s->prev;
  free(s);
}

/* --------------------- Banco / Limites --------------------- */
typedef struct {
  char* bank_name;
  int   limit_val;
  char* limit_ccy;
} BankLimit;

static BankLimit bank_limits[32];
static int bank_limit_count = 0;

static void set_bank_limit(const char* name, int val, const char* ccy) {
  if (bank_limit_count < 32) {
    bank_limits[bank_limit_count].bank_name = dupstr(name);
    bank_limits[bank_limit_count].limit_val = val;
    bank_limits[bank_limit_count].limit_ccy = dupstr(ccy);
    bank_limit_count++;
  }
}

static BankLimit* find_bank_limit(const char* name) {
  for (int i = 0; i < bank_limit_count; i++)
    if (strcmp(bank_limits[i].bank_name, name) == 0)
      return &bank_limits[i];
  return NULL;
}

/* --------------------- Loader ASM --------------------- */
static char* rstrip(char* p){ int n=strlen(p); while(n>0&&(p[n-1]=='\n'||p[n-1]=='\r')) p[--n]=0; return p; }
static int is_blank(const char* s){ while(*s){ if(!isspace((unsigned char)*s)) return 0; s++; } return 1; }
static void trim_leading(char** s){ while(**s && isspace((unsigned char)**s)) (*s)++; }

static Program load_asm(const char* path){
  Program P={0};
  FILE* f=fopen(path,"rb");
  if(!f){ perror("open .asm"); exit(1); }
  char buf[4096];
  while(fgets(buf,sizeof(buf),f)){
    rstrip(buf);
    char* line = dupstr(buf);
    P.line = realloc(P.line, (P.n+1)*sizeof(char*));
    P.line[P.n++] = line;
  }
  fclose(f);
  for(int i=0;i<P.n;i++){
    char* s=P.line[i]; trim_leading(&s);
    if(s[0]=='.' && strchr(s,':')){
      char* colon = strchr(s,':');
      size_t L = colon-s;
      char* lab = malloc(L+1);
      memcpy(lab,s,L); lab[L]=0;
      P.lab = realloc(P.lab,(P.nlab+1)*sizeof(Label));
      P.lab[P.nlab++] = (Label){ .label=lab, .pc=i };
    }
  }
  return P;
}

static int find_label(Program* P, const char* lbl){
  for(int i=0;i<P->nlab;i++)
    if(strcmp(P->lab[i].label,lbl)==0) return P->lab[i].pc;
  fprintf(stderr,"label não encontrada: %s\n",lbl);
  exit(1);
}

/* --------------------- Execução --------------------- */
static void print_val(Value v){
  switch(v.k){
    case V_INT:    printf("%d", v.v.i); break;
    case V_STR:    printf("%s", v.v.s?v.v.s:""); break;
    case V_ACCOUNT:printf("<acct:%s>", v.v.s?v.v.s:"?"); break;
    case V_MONEY:  printf("%d$%s", v.v.m.val, v.v.m.ccy?v.v.m.ccy:"???"); break;
  }
}

int main(int argc,char**argv){
  if(argc!=2){ fprintf(stderr,"uso: %s file.asm\n",argv[0]); return 2; }

  Program P=load_asm(argv[1]);
  Stack S={0};
  Env E={0};
  Snap* TX=NULL;

  for(int pc=0; pc<P.n; pc++){
    char* raw=P.line[pc];
    char* s=raw; trim_leading(&s);
    if(is_blank(s)||s[0]==';'||(s[0]=='.'&&strchr(s,':'))) continue;

    char op[64]; sscanf(s,"%63s",op);
    char* rest=s+strlen(op); while(*rest==' '||*rest=='\t') rest++;

    /* ---------------- Instruções ---------------- */
    if(strcmp(op,"PUSH_I")==0){
      int v; sscanf(rest,"%d",&v);
      Value x={.k=V_INT,.v.i=v};
      push(&S,x);
    }
    else if(strcmp(op,"PUSH_STR")==0){
      char* q1=strchr(rest,'"'); char* q2=q1?strrchr(q1,'"'):NULL;
      size_t n=q2&&q2>q1?(size_t)(q2-q1-1):0;
      char* s2=malloc(n+1); memcpy(s2,q1+1,n); s2[n]=0;
      Value x={.k=V_STR,.own=1,.v.s=s2};
      push(&S,x);
    }
    else if(strcmp(op,"PUSH_MONEY")==0){
      int v=0; char ccy[8]="";
      if(sscanf(rest,"%d , %7s",&v,ccy)!=2 && sscanf(rest,"%d %7s",&v,ccy)!=2){
        fprintf(stderr,"PUSH_MONEY malformado: %s\n",rest); exit(1);
      }
      Value x={.k=V_MONEY,.own=1};
      x.v.m.val=v; x.v.m.ccy=dupstr(ccy);
      push(&S,x);
    }
    else if(strcmp(op,"LOAD")==0){
      char name[128]; sscanf(rest,"%127s",name);
      push(&S,env_get(&E,name));
    }
    else if(strcmp(op,"STORE")==0){
      char name[128]; sscanf(rest,"%127s",name);
      Value v=pop(&S);
      env_set(&E,name,v_copy(&v));
      v_free(&v);
    }
    else if(strcmp(op,"ADD")==0||strcmp(op,"SUB")==0||strcmp(op,"MUL")==0||
        strcmp(op,"DIV")==0||strcmp(op,"MOD")==0){
        Value b=pop(&S), a=pop(&S);

        // Se qualquer operando é money, resultado deve ser money
        if (a.k==V_MONEY || b.k==V_MONEY) {
          // Normaliza: queremos operar sobre 'val' inteiro do money
          // Casos:
          //  - money (+/-) money: mesmas moedas
          //  - money (*//) int
          //  - int (+/-) money  -> comuta para money (+/-) int
          Money res = {0, NULL};

          // Escolhe moeda base: do operando money presente
          const Value* msrc = (a.k==V_MONEY) ? &a : &b;
          res.ccy = dupstr(msrc->v.m.ccy);

          // Extrai valores numéricos
          long long av = (a.k==V_MONEY) ? a.v.m.val : a.v.i;
          long long bv = (b.k==V_MONEY) ? b.v.m.val : b.v.i;

          int ok = 1;
          if (strcmp(op,"ADD")==0 || strcmp(op,"SUB")==0) {
            // Ambos precisam ser money e mesma moeda
            if (a.k!=V_MONEY || b.k!=V_MONEY ||
                strcmp(a.v.m.ccy, b.v.m.ccy)!=0) {
              ok = 0;
            } else {
              res.val = (strcmp(op,"ADD")==0) ? (int)(av + bv) : (int)(av - bv);
            }
          } else if (strcmp(op,"MUL")==0) {
            // money * int  OU  int * money
            if (a.k==V_MONEY && b.k==V_INT)      res.val = (int)(av * bv);
            else if (a.k==V_INT && b.k==V_MONEY) res.val = (int)(av * bv);
            else ok = 0;
          } else if (strcmp(op,"DIV")==0) {
            // money / int  (divisão inteira simplificada)
            if ((a.k==V_MONEY && b.k==V_INT) && bv!=0)      res.val = (int)(av / bv);
            else if ((a.k==V_INT && b.k==V_MONEY))          ok = 0;
            else if (bv==0)                                  ok = 0;
          } else if (strcmp(op,"MOD")==0) {
            // money % int (pouco comum, mas vamos permitir)
            if ((a.k==V_MONEY && b.k==V_INT) && bv!=0)      res.val = (int)(av % bv);
            else ok = 0;
          }

          v_free(&a); v_free(&b);

          if (!ok) {
            fprintf(stderr,"[VM] operação inválida com money (%s)\n", op);
            // empilha 0 para não quebrar o fluxo
            Value x={.k=V_INT,.v.i=0}; push(&S,x);
          } else {
            Value x={.k=V_MONEY,.own=1}; x.v.m = res; push(&S,x);
          }
        } else {
          // só ints
          int ai=a.v.i, bi=b.v.i, r=0;
          if(strcmp(op,"ADD")==0) r=ai+bi;
          else if(strcmp(op,"SUB")==0) r=ai-bi;
          else if(strcmp(op,"MUL")==0) r=ai*bi;
          else if(strcmp(op,"DIV")==0) r=bi?ai/bi:0;
          else if(strcmp(op,"MOD")==0) r=bi?ai%bi:0;
          v_free(&a); v_free(&b);
          Value x={.k=V_INT,.v.i=r}; push(&S,x);
        }
      }
    else if(strcmp(op,"CMP_EQ")==0||strcmp(op,"CMP_NE")==0||
            strcmp(op,"CMP_LT")==0||strcmp(op,"CMP_GT")==0||
            strcmp(op,"CMP_LE")==0||strcmp(op,"CMP_GE")==0){
      Value b=pop(&S),a=pop(&S);
      int ai=a.v.i, bi=b.v.i, r=0;
      if(strcmp(op,"CMP_LT")==0)r=ai<bi;
      else if(strcmp(op,"CMP_GT")==0)r=ai>bi;
      else if(strcmp(op,"CMP_LE")==0)r=ai<=bi;
      else if(strcmp(op,"CMP_GE")==0)r=ai>=bi;
      else if(strcmp(op,"CMP_EQ")==0)r=ai==bi;
      else if(strcmp(op,"CMP_NE")==0)r=ai!=bi;
      v_free(&a); v_free(&b);
      Value x={.k=V_INT,.v.i=r}; push(&S,x);
    }
    else if(strcmp(op,"DUP")==0){
      Value a=top(&S);
      push(&S,v_copy(&a));
    }
    else if(strcmp(op,"POP")==0){
      Value a=pop(&S); v_free(&a);
    }
    else if(strcmp(op,"JZ")==0||strcmp(op,"JNZ")==0){
      char lab[64]; sscanf(rest,"%63s",lab);
      Value a=pop(&S);
      int cond=(a.k==V_INT)?a.v.i:0;
      v_free(&a);
      if((strcmp(op,"JZ")==0 && !cond)||(strcmp(op,"JNZ")==0 && cond))
        pc=find_label(&P,lab)-1;
    }
    else if(strcmp(op,"JMP")==0){
      char lab[64]; sscanf(rest,"%63s",lab);
      pc=find_label(&P,lab)-1;
    }
    else if(strcmp(op,"CALL")==0){
      char name[128]; sscanf(rest,"%127s",name);

      if(strcmp(name,"display")==0||strcmp(name,"print")==0){
        Value a=pop(&S);
        print_val(a); printf("\n");
        v_free(&a);
      }
      else if(strncmp(name,"_open_",6)==0){
        const char* bank=name+6;
        char handle[256]; snprintf(handle,sizeof(handle),"%s:acct",bank);
        Value acct={.k=V_ACCOUNT,.own=1,.v.s=dupstr(handle)};
        push(&S,acct);

        // varre o ASM e captura o limite do banco (sem hardcode)
        for (int i=0;i<P.n;i++){
          int val=0; char ccy[4]={0};
          if (parse_limit_from_text(P.line[i], &val, ccy)) {
            set_bank_limit(bank, val, ccy);
            break;
          }
        }
      }
      else if(strcmp(name,"enforce")==0){
        Value b=pop(&S),a=pop(&S);
        v_free(&a); v_free(&b);
      }
      else if(strcmp(name,"xfer")==0){
        Value amount=pop(&S),to=pop(&S),from=pop(&S);
        printf("[VM] xfer "); print_val(amount);
        printf(" from "); print_val(from);
        printf(" to "); print_val(to); printf("\n");

        // --- checa limite dinâmico do banco ---
        if (from.k == V_ACCOUNT) {
          char* colon = strchr(from.v.s, ':');
          if (colon) {
            char bank_name[128];
            strncpy(bank_name, from.v.s, colon - from.v.s);
            bank_name[colon - from.v.s] = 0;
            BankLimit* lim = find_bank_limit(bank_name);
            if (lim && amount.k == V_MONEY &&
                strcmp(amount.v.m.ccy, lim->limit_ccy)==0) {
              if (amount.v.m.val > lim->limit_val) {
                printf("[VM] LIMITE EXCEDIDO: %d$%s > %d$%s\n",
                       amount.v.m.val, amount.v.m.ccy,
                       lim->limit_val, lim->limit_ccy);
                printf("[VM] TX_ROLLBACK\n");
                printf("Transação revertida com sucesso.\n");
                v_free(&from); v_free(&to); v_free(&amount);
                return 0;
              }
            }
          }
        }

        v_free(&from); v_free(&to); v_free(&amount);
      }
      else if(strcmp(name,"now")==0||strcmp(name,"region")==0||
              strcmp(name,"net_ok")==0||strcmp(name,"fx_rate")==0){
        Value x={.k=V_INT,.v.i=1}; push(&S,x);
      }
      else printf("[VM] CALL %s (stub)\n",name);
    }
    else if(strcmp(op,"TX_BEGIN")==0){
      TX=tx_push(TX,&E);
      printf("[VM] TX_BEGIN\n");
    }
    else if(strcmp(op,"TX_COMMIT")==0){
      printf("[VM] TX_COMMIT\n");
      tx_pop_commit(&TX);
    }
    else if(strcmp(op,"TX_ROLLBACK")==0){
      printf("[VM] TX_ROLLBACK\n");
      tx_pop_rollback(&TX,&E);
    }
    else if(strcmp(op,"RET")==0){
      break;
    }
  }

  /* limpeza final */
  for(int i=0;i<P.n;i++) free(P.line[i]);
  for(int i=0;i<P.nlab;i++) free(P.lab[i].label);
  free(P.line); free(P.lab);
  env_clear(&E);
  free(S.data);
  while(TX) tx_pop_commit(&TX);
  return 0;
}
