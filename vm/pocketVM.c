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
    case V_MONEY:  printf("%d$ %s", v.v.m.val, v.v.m.ccy?v.v.m.ccy:"???"); break;
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
      Value b=pop(&S),a=pop(&S);
      int ai=a.v.i, bi=b.v.i, r=0;
      if(strcmp(op,"ADD")==0)r=ai+bi;
      else if(strcmp(op,"SUB")==0)r=ai-bi;
      else if(strcmp(op,"MUL")==0)r=ai*bi;
      else if(strcmp(op,"DIV")==0)r=bi?ai/bi:0;
      else if(strcmp(op,"MOD")==0)r=bi?ai%bi:0;
      v_free(&a); v_free(&b);
      Value x={.k=V_INT,.v.i=r}; push(&S,x);
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
