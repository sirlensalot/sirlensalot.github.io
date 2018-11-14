% Pact: Then and Now and Then ...
% Stuart Popejoy stuart@kadena.io @SirLensALot
% Pact 2nd Birthday Anniversary

# Happy Birthday Pact!
## Where We Started

- LISP
- Keysets
- Key-value journaled database
- Upgrades
- No NULLs
- No Loops
- No Control Flow

## Pact 1.0
- Open sourced Nov 2016
- If!
- Loops! (over lists)
- map/filter/fold/compose

## Pact 2.0
- Types!
- Database schemas
- Type inference
- FV demo at Stanford

# The road to production
## Database

- Pluggable backend
- MSSQL

## Privacy

- "Pacts"
- Noise protocol (Signal, Whatsapp)

## Tooling
- `pact -s` Pact server
- Javascript library

# Actual Clients

## Database queries

```lisp
(sort 'name
  (select users user-id
    (and? (where age (> 20))
          (where role (= "admin")))))
```

## Customer demands

- `keylog`: get transaction history for a given entry in a table
- microsecond support in time objects
- Read objects and lists from JSON

# The March to Public

## Formal Verification
- Brought on lead Quorum devs
- First release June 2018
- `defproperty`
- Full language support late 2018

## Gas Model
- Architecture complete
- Fixed rate model implemented
- Public model yet to come

## Dependency Management
- Module hashes
- Pinned imports
- "Blessed" old versions
- No LeftPad! No Parity Bugs!

## Interfaces
- Real interfaces, on-chain
- "Fungible Asset" (ERC-20)
- Formal Verification Support

# Coming Soon

- Capabilities
- Generalized Governance
- Namespaces
- Row Types
- And more!

# Thank You

<http://kadena.io>

<http://github.com/kadena-io/pact>
