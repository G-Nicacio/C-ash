known issues:

# PATCHED
- this: 

            bank PocketBR {
            currency = USD;
            policy limit daily 2000$USD;
            }

            open PocketBR("Ash", email="ash@cash.dev") as br;
            reserve saldo: money = 100000$BRL;

            transaction {
            display("Tentando transferência...");
            enforce(br, saldo);
            xfer(br, br, saldo * 2); // Falha proposital (acima do limite)
            display("Esse print não deveria aparecer!");
            } onfail {
            display("Transação revertida com sucesso.");
            }

works and shouldnt (should raise an exception for different currencies, shouldnt be able to xfer automatically brl to usd account. Xfer between brl and usd should exist and should work, but not like this, before declaring at least an `if brl: brl_in_usd = brl * brl_to_usd`)

# PATCHED
- this:

            bank PocketUS {
            currency = USD;
            policy limit daily 2000$USD;
            }

            open PocketBR("Ash", email="ash@cash.dev") as br;
            reserve saldo: money = 100000$BRL;

            transaction {
            display("Tentando transferência...");
            enforce(br, saldo);
            xfer(br, br, saldo * 2); // Falha proposital (acima do limite)
            display("Esse print não deveria aparecer!");
            } onfail {
            display("Transação revertida com sucesso.");
            }

works and shouldnt (should raise an exception for 'bank PocketBR not declared')

# PATCHED
- functions dont work. this:

            fn greet(name) {
                display('Hello, ');
                display(name);
            }
            
            reserve nameEx: string = 'Ash';
            greet(nameEx);

doesnt display anything on bash. should display:
#
Hello,\
Ash
#
