% Smart Contracts and Formal Verification In Pact
% Stuart Popejoy stuart@kadena.io @SirLensALot
% Construct 2017

# Smart Contracts & Formal Verification in Pact

## What Are Smart Contracts?

- Event Representation
- Invariant Enforcement
- Not general purpose computers

## Safety & Correctness

- Blockchains & Culture of Correctness
- Smart Contract Errors are Costly
- Bugs == Vulns

## Finding Bugs

- Types
- Unit Tests
- Static Analysis (FindBugz)
- Formal Verification

## Unit Tests

- Test Creep
- You can only test what you know

## Formal Verification

> Formal verification is the act of proving or disproving the correctness of intended algorithms underlying a system with respect to a certain formal specification or property, using formal methods of mathematics.


## Formal Verification In Use

- TLA+ @ Amazon
- Cryptol/Z3 @ Galois
- Coq & Isabelle @ Academia
- Z3 & Pact


# Introduction to Pact

## Pact Basics

- Interpreted
- DB-focused, backend-agnostic
- Turing-incomplete
- Single-assignment
- Type inference

## System architecture

![](img/pact/pact-system-diagram.png)

## A simple smart contract

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

## "Key-Row" Structure

- Key-Value with field access, schema
- Mutable metaphor ("latest value")
- Direct, JSON-like representation in code

## Automatic versioning

![All updates recorded with tx id](img/pact/pact-versioned-db.png)

## RDBMS back end

- Kadena defaults to SQLite (fast)
- Data is "trapped in the blockchain"
- Plug in Oracle, DB2, Postgres

# Developing with Pact

- [Web Editor](http://kadena.io/try-pact)
- Atom integration with linter
- Write/test/verify before you deploy on-chain

# Formal Verification with Z3

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

## What's Next

- Full Datatype Support
- More DSL Cases
- Richer operators

# Thank You

Stuart Popejoy stuart@kadena.io @SirLensALot

Pact Github [https://github.com/kadena-io/pact](https://github.com/kadena-io/pact)

Pact site [http://kadena.io/pact](http://kadena.io/pact)

Web editor [http://kadena.io/try-pact](http://kadena.io/try-pact)

[http://slpopejoy.github.io/talks/construct-17-pact.html](http://slpopejoy.github.io/talks/construct-17-pact.html)
