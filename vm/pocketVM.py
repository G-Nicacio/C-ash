# pocketVM.py — PocketASM VM (funções, validações e currency checks)
import sys, re

K_INT, K_STR, K_ACCOUNT, K_MONEY = "int", "str", "account", "money"

def v_int(i): return {"k": K_INT, "v": i}
def v_str(s): return {"k": K_STR, "v": s}
def v_acct(s): return {"k": K_ACCOUNT, "v": s}
def v_money(v,c): return {"k": K_MONEY, "v": {"val": v, "ccy": c}}
def v_copy(v): return {"k": v["k"], "v": v["v"].copy() if isinstance(v["v"], dict) else v["v"]}

class Stack:
    def __init__(self): self.data = []
    def push(self,v): self.data.append(v)
    def pop(self):
        if not self.data: raise IndexError("stack underflow")
        return self.data.pop()
    def top(self):
        if not self.data: raise IndexError("stack empty")
        return self.data[-1]
    def __len__(self): return len(self.data)

class Env:
    def __init__(self): self.map = {}
    def get(self,name): return v_copy(self.map.get(name, v_int(0)))
    def set(self,name,v): self.map[name]=v_copy(v)
    def clone(self):
        e=Env()
        e.map={k:v_copy(v) for k,v in self.map.items()}
        return e

class Snap:
    def __init__(self,env): self.env=env.clone(); self.prev=None

class Program:
    def __init__(self):
        self.lines=[]
        self.labels={}
        self.bank_limits={}
        self.bank_currency={}
        self.declared_banks=set()
        self.func_labels={}

def trim(s): return s.strip()

def load_asm(path):
    P=Program()
    with open(path, encoding="utf-8") as f:
        P.lines=[l.rstrip("\r\n") for l in f]

    current_bank = None
    for i,raw in enumerate(P.lines):
        s=trim(raw)
        if s.startswith(".") and ":" in s:
            lab=s.split(":",1)[0]
            P.labels[lab]=i

        # bancos e políticas
        if s.startswith("; BANK"):
            m=re.match(r";\s*BANK\s+(\w+)\s+currency\s*=\s*([A-Z]{3})",s)
            if m:
                name,ccy=m.groups()
                current_bank = name
                P.declared_banks.add(name)
                P.bank_currency[name]=ccy
                continue

        if s.startswith("; POLICY limit"):
            m=re.match(r";\s*POLICY\s+limit\s+\S+\s+(\d+)\$ ?([A-Z]{3})",s)
            if m and current_bank:
                P.bank_limits[current_bank]=(int(m.group(1)),m.group(2))
                continue

        # funções
        if s.startswith("; FN "):
            m=re.match(r";\s*FN\s+(\w+)",s)
            if m:
                fn=m.group(1)
                for j in range(i, len(P.lines)):
                    if P.lines[j].strip().startswith(f".FN_{fn}:"):
                        P.func_labels[fn]=j
                        break
    return P

def print_val(v):
    k=v["k"]
    if k==K_INT: print(v["v"], end="")
    elif k==K_STR: print(v["v"], end="")
    elif k==K_ACCOUNT: print(f"<acct:{v['v']}>", end="")
    elif k==K_MONEY: print(f"{v['v']['val']}${v['v']['ccy']}", end="")

def parse_str(rest):
    q1, q2 = rest.find('"'), rest.rfind('"')
    return rest[q1+1:q2] if q1>=0 and q2>q1 else ""

def parse_money(rest):
    m=re.match(r"\s*(\d+)\s*[,$ ]\s*([A-Za-z]{3})",rest)
    if not m: raise RuntimeError(f"[VM] PUSH_MONEY malformado: {rest}")
    return int(m.group(1)), m.group(2)

# ... (cabeçalho igual)

class Frame:
    def __init__(self, ret_pc, prev_env, args):
        self.ret_pc = ret_pc
        self.prev_env = prev_env
        self.args = list(args)

def run(path):
    P = load_asm(path)
    S = Stack()
    E = Env()
    TX = None
    call_frames = [] 

    pc = 0
    n = len(P.lines)

    skip_mode = False
    for i, line in enumerate(P.lines):
        s = trim(line)
        if not s or s.startswith(";"):
            continue

        if s.startswith(".FN_"):
            skip_mode = True
            continue

        if skip_mode:
            if s.startswith("RET"):
                skip_mode = False
            continue

        if not s.startswith(".") and not s.startswith(";"):
            pc = i
            break

    while pc < n:
        raw = P.lines[pc]
        s = trim(raw)
        pc += 1

        if not s or s.startswith(";"): 
            continue
        if s.startswith(".") and ":" in s:
            continue

        parts = s.split(None, 1)
        op = parts[0]
        rest = parts[1].split(";")[0].strip() if len(parts) > 1 else ""

        try:
            if op == "PUSH_I":
                S.push(v_int(int(rest) if rest else 0))

            elif op == "PUSH_STR":
                S.push(v_str(parse_str(rest)))

            elif op == "PUSH_MONEY":
                val, ccy = parse_money(rest)
                S.push(v_money(val, ccy))

            elif op == "LOAD":
                S.push(E.get(rest))

            elif op == "STORE":
                E.set(rest, S.pop())

            elif op in ("ADD","SUB","MUL","DIV","MOD"):
                b, a = S.pop(), S.pop()
                if a["k"]==K_MONEY or b["k"]==K_MONEY:
                    if a["k"]!=b["k"] or a["v"]["ccy"]!=b["v"]["ccy"]:
                        raise RuntimeError("[VM] operação entre moedas diferentes.")
                    av, bv = a["v"]["val"], b["v"]["val"]
                    if op=="ADD":   S.push(v_money(av+bv, a["v"]["ccy"]))
                    elif op=="SUB": S.push(v_money(av-bv, a["v"]["ccy"]))
                    elif op=="MUL": S.push(v_money(av*bv, a["v"]["ccy"]))
                    elif op=="DIV": S.push(v_money(av//bv if bv else 0, a["v"]["ccy"]))
                    else:           S.push(v_money(av%bv if bv else 0, a["v"]["ccy"]))
                else:
                    av, bv = a["v"], b["v"]
                    if op=="ADD":   S.push(v_int(av+bv))
                    elif op=="SUB": S.push(v_int(av-bv))
                    elif op=="MUL": S.push(v_int(av*bv))
                    elif op=="DIV": S.push(v_int(av//bv if bv else 0))
                    else:           S.push(v_int(av%bv if bv else 0))

            elif op in ("JZ","JNZ"):
                lab = rest
                cond = S.pop()
                c = cond["v"] if cond["k"] == K_INT else 0
                if (op=="JZ" and not c) or (op=="JNZ" and c):
                    pc = P.labels.get(lab, pc)

            elif op == "JMP":
                pc = P.labels.get(rest, pc)

            elif op == "TX_BEGIN":
                snap = Snap(E); snap.prev = TX; TX = snap
                print("[VM] TX_BEGIN")

            elif op == "TX_COMMIT":
                print("[VM] TX_COMMIT")
                TX = TX.prev if TX else None

            elif op == "TX_ROLLBACK":
                print("[VM] TX_ROLLBACK")
                if TX:
                    E = TX.env.clone()
                    TX = TX.prev

            elif op == "CALLFN":
                m = re.match(r"(\w+)\s+(\d+)", rest)
                if not m:
                    raise RuntimeError(f"[VM] CALLFN malformado: {rest}")
                name, argc_s = m.groups()
                argc = int(argc_s)

                tmp = []
                for _ in range(argc):
                    tmp.append(S.pop())
                args = list(reversed(tmp))

                if name not in P.func_labels:
                    print(f"[VM] CALL {name} (stub)")
                    continue

                fr = Frame(ret_pc=pc, prev_env=E, args=args)
                call_frames.append(fr)
                E = Env()
                pc = P.func_labels[name] + 1
                continue

            elif op == "PUSH_ARG":
                idx = int(rest) if rest else 0
                if not call_frames:
                    raise RuntimeError("PUSH_ARG fora de função")
                fr = call_frames[-1]
                if idx < 0 or idx >= len(fr.args):
                    S.push(v_int(0))
                else:
                    S.push(v_copy(fr.args[idx]))

            elif op == "CALL":
                name = rest

                if name in ("display","print"):
                    a = S.pop()
                    print_val(a); print()
                    continue

                if name.startswith("_open_"):
                    bank = name[6:]
                    if bank not in P.declared_banks:
                        print(f"[VM] ERRO: banco '{bank}' não declarado.")
                        return 1
                    S.push(v_acct(f"{bank}:acct"))
                    continue

                if name == "enforce":
                    _b, _a = S.pop(), S.pop()
                    continue

                if name == "xfer":
                    amount, to, from_ = S.pop(), S.pop(), S.pop()
                    print(f"[VM] xfer {amount['v']['val']}${amount['v']['ccy']} from {from_['v']} to {to['v']}")
                    if from_["k"] == K_ACCOUNT:
                        bank = from_["v"].split(":",1)[0]
                        if bank not in P.bank_currency:
                            print(f"[VM] ERRO: banco '{bank}' sem currency declarada.")
                            return 1
                        bank_ccy = P.bank_currency[bank]
                        if amount["v"]["ccy"] != bank_ccy:
                            print(f"[VM] ERRO: não é possível transferir {amount['v']['val']}${amount['v']['ccy']} de uma conta {bank_ccy} sem conversão explícita.")
                            return 1
                        lim = P.bank_limits.get(bank)
                        if lim and amount["v"]["val"] > lim[0]:
                            print(f"[VM] LIMITE EXCEDIDO: {amount['v']['val']}${amount['v']['ccy']} > {lim[0]}${lim[1]}")
                            print("[VM] TX_ROLLBACK")
                            print("Transação revertida com sucesso.")
                            return 0
                    continue

                if name in ("now","region","net_ok","fx_rate"):
                    S.push(v_int(1))
                    continue

                print(f"[VM] CALL {name} (stub)")

            elif op == "RET":
                if call_frames:
                    fr = call_frames.pop()
                    pc = fr.ret_pc
                    E = fr.prev_env
                else:
                    break

        except Exception as e:
            sys.stderr.write(f"[VM] erro em '{raw}': {e}\n")
            return 1

    return 0


if __name__=="__main__":
    if len(sys.argv)!=2:
        print(f"uso: {sys.argv[0]} file.asm")
        sys.exit(2)
    sys.exit(run(sys.argv[1]))
