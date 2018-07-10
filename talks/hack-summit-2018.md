% Formal Verification, Governance and Safety on the Blockchain
% Stuart Popejoy stuart@kadena.io @SirLensALot
% hack.summit("blockchain") 2018


# Smart Contracts

## Need to be Safe

- Smart-contract blockchains: automation with live ammo
- Ethereum: numerous bugs/exploits with costs in the $MM
- Developers are not superheroes
- Tests are not enough

## Need to be Simple

- Simple means **easy to understand**
- Simple means **fast to code**
- Simple means **safer**
- Simple means **other stakeholders can understand it**

## Need to be Maintainable

- Upgrades are essential
- Data migration is essential
- Per-contract governance
- Contracts need to be interoperable

# Formal Verification

## Tests only test what you know

- Developers must think of every scenario
- Fuzzers, Quickcheck are probabilistic

## Use a Theorem Prover, Hard Mode

- Full specification: entire language is simulated by prover
- Proofs are written in the Prover's dialect (Coq, SMTLIB2 etc)
- PhD please

## Use a Theorem Prover, Easy Mode

- Model checking: instrument code with small proofs
- Provide a DSL that is easy to read and write
- Prove what is most important (DB state, authorization, etc)
- No PhDs required

## Demo of Pact Model Checking

```lisp
  (defschema account
    ("Row type for accounts table."
      (invariant (>= balance 0.0)))
     balance:decimal
     amount:decimal
     ccy:string
     )
```

# Governance and Maintainability

## Maintaining Code on the Blockchain

- Requires upgrades!
- Requires data migration
- Requires a name/directory based approach, not addresses
- Requires governance

## Governance in Pact Today

- Public-key based
- Multisig friendly, like all of Pact
- Allows upgrades
- Allows direct data manipulation
```lisp
(module inventory 'inventory-admins-keyset
  "Inventory component"
  (defschema inventory-record
    ...)
  (deftable inventory:{inventory-record})
...
)
```

## Governance in Pact Tomorrow

- Fully general governance
- Decentralized: collect votes on hash of upgrade tx
- More complex keyset regimes
```lisp
(module inventory inventory-governance
...
  (defun inventory-governance ()
    (tally-votes (tx-hash)))
...
)
```

# Safety in Smart Contracts

## Ethereum: Hard to Write Safe Code

- EVM: Fully general machine model
- "Run the Contract and See What Happens"
- No multisig support
- **slow** development time

## Pact: Safe by Design

- No recursion (source of DAO bug)
- SQL-like instead of OO-like (source of Rubixi bug)
- Inlined dependencies (source of Parity Wallet bug)

## Pact: Safe means Easier, Faster

- More built-in functionality, standard library
- Pacts: easy multi-step transactions, avoid bugs
- Multisig support built-in
- Simple, easy database model

## Demo of Escrow Pact

```lisp
  (defpact two-party-escrow (deb-acct cred-acct
                             escrow-amount:decimal timeout)
    "Simple two-party escrow pact"
    (step-with-rollback
      (init-escrow deb-acct escrow-amount)
      (cancel-escrow timeout deb-acct cred-acct escrow-amount))
    (step
      (finish-escrow deb-acct cred-acct
                     escrow-amount)))
```






# Thank You!

Stuart Popejoy stuart@kadena.io @SirLensALot

Pact Github <https://github.com/kadena-io/pact>

Pact site <http://kadena.io/pact>

Web editor <http://kadena.io/try-pact>

This talk <http://slpopejoy.github.io/talks/hack-summit-2018.html>
