# register allocation
- we can assign an aloc to a register when:
- the register's current value is no longer neded
- the register's current value is not "in conflict" w/ a new value
- when aloc is actually needed

## our first big optimization!

### undead-analysis
- figure out which abstract locations are not definitely dead??
- we previously talked about values, but we can't know all of them statically?

### conflict analysis
- figure out which locations cant be assigned different registers
- 2 locations that are not dead at the same time are in conflict

### assign-registers
- assign arbitrary non-conflicting registers to all live abstract locs
- dump everything else into a frame.

### a location is undead between a defn' and a reference
e.g.:
```
(set! x.1 0)
. undead
. undead
.
(set! x.2 x.1) ; say x.1 is done here, final ref
.
. ; dead
.
(set1 x.1 1) ; undead again
```

### the algorithm
- start from end of program
- when we see a reference, consider the location alive
- when we see a definition, we "kill" a location and mark it dead
### the data structure
- we'll need a tree of sets to map instructions to sets