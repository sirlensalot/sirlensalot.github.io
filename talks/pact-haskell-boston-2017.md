% Pact: A Smart-Contract Language in Haskell
% Stuart Popejoy stuart@kadena.io @SirLensALot
% Boston Haskell March 2017


# Smart Contracts

## What Aren't They?

- Autonomous orgs/AI
- Lawyers
- "Experts Only" code
- Hard forks/binary installs

## What Are They?

- User code
- Mobile code/code-as-data
- Event representation
- Authentication/Authorization
- As simple/safe as possible

## How?

- Database metaphor
- Public-key auth support
- "Just enough" computation
- Human-readable
- Testability/Verification


# Introduction to Pact

## Pact Basics

- Interpreted
- DB-focused, backend-agnostic
- Turing-incomplete
- Single-assignment
- Type inference

## System architecture

![](img/pact/pact-system-diagram.png)

## Smart Contract == (Modules|Tables|Keysets)

```{.commonlisp}
(define-keyset 'employees-admin (read-keyset "admin-ks"))

(module employees 'employees-admin ;; admin keyset

  (defschema employee
    name:string
    age:integer
    salary:decimal)

  (deftable 'employees:{employee})

  (defun add-employee (id name age salary)
    (enforce-keyset 'empl-operator) ;; biz keyset
    (insert employees id
      { "name": name, "age": age, "salary": salary }))
)
```

# Database Metaphor

## Concepts

- OLTP good (but not enough)
- OLAP bad (but need latest value)
- Versioning good
- One lang to rule them all

## "Key-Row" Structure

- Key-Value with field access, schema
- Mutable metaphor ("latest value")
- Direct, JSON-like representation in code

## Automatic versioning

![`(update accounts "A" { "balance": 18500.0, "amount": -1500.0})`<br>`(update accounts "B" { "balance": 1500.0,  "amount": 1500.0 })`](img/pact/pact-versioned-db.png)

## RDBMS back end

- Kadena defaults to SQLite (fast)
- Data is "trapped in the blockchain"
- Plug in Oracle, DB2, Postgres

# Developing with Pact

- [Web Editor](http://kadena.io/try-pact)
- Full database-backed dev environment (`pact -serve`)
- Atom integration with linter: continuous testing


# Formal Verification with Z3


## Safety & Correctness in Smart Contracts

- Blockchains & Culture of Correctness
- Smart Contract Errors are Costly
- Bugs == Vulns

## Finding Bugs

- Types
- Unit Tests
- Static Analysis (FindBugz)
- Formal Verification

## Formal Verification In Use

- TLA+ @ Amazon
- Cryptol/Z3 @ Galois
- Z3 & Pact
- Many More, but not a ton of industrial use

## Pact makes proving ~~easy~~ tractable

- Already SSA
- No recursion
- Already "inlined"
- Typechecker outputs typed AST

## &nbsp;

![](img/pact/pactToZ3-1000.png)

## But what do we prove?

- DB is a big, mutable global variable
- Let's track a column
- Does it stay within range?
- Does it "conserve mass"?

## "DocTest" proof specification

```{.commonlisp}
(defun pay (from to amount)

  "Transfer money between accounts \
  \{-# PROVE 'accounts.balance' [ConservesMass, Column >= 0] #-}"

  (with-read accounts from { "balance":= from-bal }
    (with-read accounts to { "balance":= to-bal }
      (enforce (>= from-bal amount) "Insufficient Funds")
      (update accounts from
        { "balance": (- from-bal amount) })
      (update accounts to
        { "balance": (+ to-bal amount) }))))
```

## Demo

Pact -> SMT-LIB2 -> Z3

## What's Next For Formal Verification

- Full Datatype Support
- More DSL Cases
- Richer operators

# Haskell Implementation Notes

## Runtime

- `parsers` (`attoparsec` w/ tokens!): `Text` -> `Exp`
- Compilation: `Exp` -> `Term Name`, with `bound`
- Module load: `Term Name` -> `Term Ref`
- `LANGUAGE GenericNewTypeDeriving` for days

## Back-end

- "Funrecs" instead of typeclasses
- Fun with production DBs
- `MVar`s instead of `State`
- Exceptions (`IO`) instead of `MonadError`

## Typechecker

- Further lowering: `Term Ref` (inlined) -> `AST UserType`
- Natives and overloads
- Homebrew algo -> semi-HM (binary substitution)

## Z3 compiler

- Consumes typechecker AST (must fully typecheck)
- `SmtLib` for issuing SMT-LIB2

## Haskell Advantage

- Faster, rock-solid refactors
- PL tooling (bound, parsers, SmtLib)
- Exposure to formal verification
- GHCJS
- Hubris & Intimidation

# Thank You

Stuart Popejoy stuart@kadena.io @SirLensALot

Pact Github [https://github.com/kadena-io/pact](https://github.com/kadena-io/pact)

Pact site [http://kadena.io/pact](http://kadena.io/pact)

Web editor [http://kadena.io/try-pact](http://kadena.io/try-pact)

[http://slpopejoy.github.io/talks/pact-haskell-boston-2017.html](http://slpopejoy.github.io/talks/pact-haskell-boston-2017.html)
