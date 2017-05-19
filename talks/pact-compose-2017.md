% Smart Contracts and Formal Verification with Pact
% Stuart Popejoy stuart@kadena.io @SirLensALot
% Compose Conference 2017


# Why Smart Contracts

## &nbsp;

- Make a simple solution
- Make a simple service
- Run on a trustless system
- Make an API
- Make money

# Why a New Language

## &nbsp;

- Fit to purpose: Authorization
- Fit to purpose: Database
- Reduce surface area for attacks
- Reduce difficulty
- Speed solution, API dev

## Why not Solidity

- Unsafe: compiled
- Unsafe: general-purpose
- OO vs SQL
- Unsafe: Mutability
- Hard: APIs, RPC etc
- DAO attack, december vuln, etc.

## Why Pact

- Safer: Interpreted
- Safer: Turing-incomplete
- Simpler: Auth + SQL
- Safer: "Single Assignment"
- Easier: APIs, Services, etc

# Pact

## Interpreted

- Better for "live-code environments"
- LISP for minimal lowering
- REPL-powered development
- Types??

## Untyped

- "Biz-readable"
- "Devs don't like types"
- Pact 1.0

## Type Inference

- SMT-LIB2: We Must Have Types
- H-M: We Can Haz Types
- But we're interpreted ... ?

## "Just Enough Types"

- Expose typechecker to tooling
- Use for formal verification
- Enforce only declared types
- In practice like SQL

## Safety

- Interpreted vs compiled
- "Single assignment"
- Turing-incomplete (no recursion, unbounded loops)
- No catching of exceptions

## DSL vs Lang

- Auth DSL: Keysets
- Biz DSL: Rule Enforcement
- Key-Value DB lang ...
- ... extended to modules (`bind`)
- ... and reusable computation

# Writing Pact in Haskell

## General Considerations

- Best/Easiest Lang Lang: Haskell
- But Not Everybody Gets Haskell
- Don't Write Haskell in Haskell

## Types

- `Term a`: Monad/Traversable + Bound
- Parse: Text -> `Exp`
- Compile: `Exp` -> `Term Name`
- Resolve: `Term Name` -> `Eval e (Term Ref)`
- Eval: `Term Name` -> `Eval e (Term Name)`
- Typecheck: `Term Ref` -> `AST UserType`
- Solver: `AST UserType` -> SMT-LIB2

## Extensibility

- `Eval e a`: Parameterized over backend state
- Execute a `PactDb e`
- Performance problems with MTL-style typeclass

## Libraries

- `bound`
- `trifecta` (`TokenParsing`,`DeltaParsing`)
- `ed25519-donna`
- `cacophony` (`Curve25519`)
- `blake2`
- `snap`
- `direct-sqlite`

# Pact Design

## System architecture

![](img/pact/pact-system-diagram.png)

## &nbsp;

```{.commonlisp}
(define-keyset 'keyset-admin (read-keyset "ks-admin"))
(define-keyset 'keyset-operator (read-keyset "ks-op"))

(module employees 'keyset-admin
  "Employee management smart contract."
  (defschema employee name:string
                      age:integer
                      salary:decimal)

  (deftable employees:{employee})

  (defun add-employee (id name age salary)
    (enforce-keyset 'keyset-operator)
    (insert employees id
      { "name": name, "age": age, "salary": salary })))
(create-table employees)
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
- Instant API-ification

## Automatic versioning

![`(update accounts "A" { "balance": 18500.0, "amount": -1500.0})`<br>`(update accounts "B" { "balance": 1500.0,  "amount": 1500.0 })`](img/pact/pact-versioned-db.png)

## RDBMS back end

- Kadena defaults to SQLite (fast)
- Data is "trapped in the blockchain"
- Plug in Oracle, DB2, Postgres
- Build your own

# Formal Verification with Z3

## Pact makes proving ~~easy~~ tractable

- Already "SSA"
- No recursion
- Already "inlined"
- Typechecker outputs typed AST

## &nbsp;

![](img/pact/pactToZ3-1000.png)

## Formal Verification To The Masses

- What do we prove??
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

## What's Next For Formal Verification

- Full Datatype Support
- More DSL Cases
- Richer operators

# Thank You

Stuart Popejoy stuart@kadena.io @SirLensALot

Pact Github [https://github.com/kadena-io/pact](https://github.com/kadena-io/pact)

Pact site [http://kadena.io/pact](http://kadena.io/pact)

Web editor [http://kadena.io/try-pact](http://kadena.io/try-pact)

[http://slpopejoy.github.io/talks/pact-compose-2017.html](http://slpopejoy.github.io/talks/pact-compose-2017.html)
