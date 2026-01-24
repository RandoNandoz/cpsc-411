# IRs in 411
- Monaidc form

- Transforms exprs w/ controlflow into statements w/ dataflow

- ANF -- A Normal Form, a kind of monadic normal form
- Transforms exprs w/ control flow into statements and data flow
- Challenging w/ branching

- CPS
- Turns code into jumps (bad for pipeline??)
- Hard to compile

- ANF
- Serialize our lets
- Easy to read, hard to optimize/messed up to compile
- Branching explodes

- Monadic form

- Imperative A-normalization of Monadic Form
- Think w/ contiuations
- Traverse until we find the first operation
- Build up the cont's
- 

