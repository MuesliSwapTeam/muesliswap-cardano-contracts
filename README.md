# MuesliSwap Cardano Smart Contracts 🥣

The contracts that power the order book model of MuesliSwap for Cardano, written
in Plutus V1. [1]
For a short introduction into the programming language, check out [2].
The protocol design is based on highly developed order book models
as present in CEXs since early days of stock trading. [3]

## Design Outline

### Placing orders

An order is placed by sending an amount of ADA and assets to
the script address [3].
This will lock the sent funds in the contract.

To unlock the funds, one of two actions can be chosen.
Each action can only be conducted if the script is provided with UTxOs fulfilling the requirements defined
in the additional information.
Further, the outputs must fully consume all locked funds.

### Canceling orders

To cancel an order, the transaction that asks for the refund
needs to be signed by the original creator of the order.
The design of order canceling makes it impossible for any third party,
to maliciously withdraw funds from the script without approval of the person that placed the order.

### Matching orders

If orders that match according to locked funds and additional trade information, they can be matched.
The script ensures that each party gets credited the amount
that was ordered in the initially placed order.
That way the matchmaker can decide to claim any remaining
difference as reward.
Any party can act as the matchmaker, as long as it proves to the script
that all participants in the trade receive what they ordered.


# Build instructions

In order to verify the contract is indeed the one used on-chain, you need to build the project and compute the address of the script yourself. To do so, first compile via

```bash
cabal build order-validator
```

and then run

```bash
cabal run order-validator
```

to produce the 'order_validator_v1.1.plutus' file. To get the corresponding address via the cardano CLI, run

```bash
cardano-cli address build --payment-script-file order_validator_v1.1.plutus --mainnet --out-file order_validator_v1.1.addr
```



# References

- [1] Plutus Programming Language Overview: https://testnets.cardano.org/en/programming-languages/plutus/overview/
- [2] Plutus Pioneer Program: https://github.com/input-output-hk/plutus-pioneer-program
- [3] How to Build a Fast Limit Order Book: https://web.archive.org/web/20110219163448/http://howtohft.wordpress.com/2011/02/15/how-to-build-a-fast-limit-order-book/
- [4] check `order_validator_v1.1.addr` for the address of the script
- [5] https://githubmemory.com/repo/input-output-hk/cardano-addresses
