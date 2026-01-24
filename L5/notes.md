# 2025-01-20

When we work with languages, we think about equivalence...

Equality vs. equivalence

- Identical -> i.e., pointer equivalent
- Structural equality -> same structure, base values are identitcal
- Equivalence -> Differing structure but the same interpretation, e.g., `(set= '(a a b) '(a b))`

- Effectful programs -> hard to decide what the "effect" is, for example, heat created on the CPU, time is probably
  not an effect we are concerned with

- Two expressions are equivalent when they represent the same values in all contexts, with all possible values 
- We can't decide this, it's very hard to do this for turing complete languages

- Okay, what about statements?
- The statements leave the machine in indistinguishable states, in all contexts
- ?? timing ?? CPU ?? well no

- Important: expression/statement distinction
- 

