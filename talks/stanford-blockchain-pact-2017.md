% Pact: A LISP for Smart Contracts and "Pervasive Determinism"
% Stuart Popejoy stuart@kadena.io
% January 2017

# Pact: A LISP for Smart Contracts

## About Kadena

- Founded in 2016 from JPM blockchain group
- Tangaroa -> Juno
- ScalableBFT, the "first real private blockchain"
- Masala, Hopper ... Pact

## Pervasive Determinism

- Consensus
- Commit w/ Immediate Confirmation
- Transaction Execution (Pact)

## Roadmap

- Smart Contracts
- Pact Overview
- Database Metaphor
- Public-key Auth
- Safer Contracts
- Confidentiality
- Types & Z3/Prover preview


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


# Pact Overview

## Language
- Interpreted LISP
- Turing-incomplete
- Single-assignment
- DB-focused, backend-agnostic
- Type inference

## System architecture

![](img/pact/pact-system-diagram.png)

## System Requirements: DL Front end

- Total ordering
- Single-threaded
- Signatures verified, Multi-sig support
- Provide ordered transaction ID

## System Requirements: DB Back end

- Fast
- Key-Value/JSON support

## Modules + Tables + Keysets

- A `module` defines functions, types and tables
- Keysets guard admin and business operations

## Modules + Tables + Keysets

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

## Special binding form: `with-read`

```{.commonlisp}

(defun read-balance (acct-key)

  (with-read accounts acct-key
    {
      "balance" := bal
    , "ccy"     := ccy
    }

    (format "balance for {}: {} {}" acct-key bal ccy))
)

> (read-balance "stu")
"balance for stu: 100.0 USD"

```

## Automatic versioning

![All updates recorded with tx id](img/pact/pact-versioned-db.png)

## No `NULL`s

- Violates relational calculus ;)
- Enforces totality
- Avoids control flow

## No `NULL`s

Missing rows OK though!

<div class="sourceCode"><pre class="sourceCode"><code class="sourceCode commonlisp">
  (<span class="kw">with-read-default</span> inventory inv-item
    { <span class="st">"count"</span> := count } <span class="co">; variable binding</span>
    { <span class="st">"count"</span> : 0 }      <span class="co">; default row value</span>
    (format <span class="st">"found {} widgets"</span> count)
  )

</code></pre></div>

## RDBMS back end

- Kadena defaults to SQLite (fast)
- Data is "trapped in the blockchain"
- Plug in Oracle, DB2, Postgres

# Public-key Auth

## Concepts

- Inspired by Bitcoin scripts
- Code doesn't verify but enforces matches
- `keyset` concept combines a set of keys and a rule
- Table access within module is unguarded

## &nbsp;


```{.json}
/* JSON part of message. keys-2 requires at least 2 keys to match */
{ "keyset": { "keys": ["CEO","CTO","Mom"], "pred": "keys-2" } }
```
```{.commonlisp}
(define-keyset 'admin-keyset (read-keyset "keyset"))

(module bonus 'admin-keyset

  (deftable bonus)

  (defun supersize-bonus (me)
    (enforce-keyset 'admin-keyset) ;; must be admin
    (update bonus me { "amount": 1000000.0 })
  )

  ;;anybody can view bonuses!
  (defun read-bonus (id) (read bonus id))
)
```
## Row-level keysets

Keysets can be stored in the database and used for "row-level" auth.

```{.commonlisp}

(defun update-ssn (id ssn)

  (with-read persons id
      { "keyset" := ks }
    (enforce-keyset ks) ; enforce against keyset in database

    (update persons id { "ssn": ssn }))
)

```

# Safer Contracts

## "Just enough" Computation

- No recursion (allows inlining)
- Looping only over (non-infinite) lists
- Single assignment

## Human-readable

- Interpreted vs compiled
- LISP "Just the AST please"
- Modules not addresses
- "Module" == "Smart Contract API"

## "Just enough" typing

![](img/pact/pact-typed-or-not.png)

## "Just enough" typing

- Rapid prototype without types, or just schemas
- Typechecker helps development
- Runtime only enforces declared types
- TC necessary for prover

# Confidentiality

## &nbsp;

![](img/pact/confidentiality-sysdiagram.png)

## Disjoint Databases

![In a "single-chain" confidentiality configuration, obscured transactions in the ledger do not have corresponding entries in the smart contract database.](img/pact/disjoint-dbs.png)

## "Pacts" (Coroutines)

```{.commonlisp}
(defpact pay (payer payer-entity
              payee payee-entity amount date)

  ; step 1: debit from payer
  (step-with-rollback payer-entity
    (debit payer amount date { "payee": payee })
    ; rollback if step 2 fails
    (credit payer amount date))

  ; step 2: credit to payee
  (step payee-entity
    (credit payee amount date { "payer": payer }))
)
```

## &nbsp;

![](img/pact/pact-execution.png)

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

Stuart Popejoy

Pact Github [https://github.com/kadena-io/pact](https://github.com/kadena-io/pact)

Pact site [http://kadena.io/pact](http://kadena.io/pact)

Web editor [http::/kadena.io/try-pact](http::/kadena.io/try-pact)

[http://slpopejoy.github.io/talks/stanford-blockchain-pact-2017.html](http://slpopejoy.github.io/talks/stanford-blockchain-pact-2017.html)
