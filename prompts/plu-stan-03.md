I need help identifying and fixing PLU-STAN-03 anti-pattern violations in my Plutus smart contract code.

**PLU-STAN-03 Rule:** "No usage of Optional types in on-chain code"
**Description:** No use of Maybe or Either types in on-chain code. On-chain code should fail fast with clear errors instead of hiding failures in optional types.

## What to look for:
1. Function calls to detect:
   - `fromMaybe` (especially from `PlutusTx.Maybe` module)
   - Functions that return `Maybe` or `Either` types in on-chain code
   - `find`, `lookup`, `headMaybe`, `listToMaybe` in validator contexts

2. Type signatures to flag:
   - `Maybe a` in function parameters or return types
   - `Either e a` in on-chain functions
   - Type aliases or newtypes wrapping Maybe/Either

3. Pattern matches to review:
   - `case mx of { Just x -> ...; Nothing -> ... }`
   - Pattern matching on `Right`/`Left`

## My code (paste your code here):
[PASTE YOUR PLUTUS/HASKELL CODE HERE]

## What I need from you:
1. Scan my code and identify all PLU-STAN-03 violations
2. For each violation found:
   - Show the exact line/location
   - Explain why it's problematic
   - Suggest a specific fix using fast-fail alternatives

3. Suggested replacements:
   - Instead of `fromMaybe default maybeValue`, use explicit error handling with `traceError`
   - Instead of `find predicate list`, use `tryFind` or similar fast-fail variant
   - Instead of returning `Maybe a`, return `a` with explicit error cases
   - Instead of `Either e a`, use error tracing directly

4. Show me examples of how to rewrite the problematic code