# ToyStory 🧸

Your task is to calculate various statistics on the set of toys in one loop. We are assuming a lot of records (e.g. from database), so we don't want to loop over the data multiple times. For the simplicity, we are using a list of toys instead of a database. You can use data from the file `toys.csv` - but you don't have to load it in the application, you can just copy the data to the code.

Statistics to calculate:

- total number of toys
- total number of blue toys (`color == "blue"`)
- distribution of toys by color (`color->count`)
- total number of broken toys with a price higher than 10 (`broken && price > 10`). The threshold price is configurable.

Requirements:

- calculate all statistics in one loop (simulates a lot of records).
- there is a _lot_ of statistics to calculate, even though we are using only few in the example.
- adding a new statistic must be code-driven as much as possible.
- the result is a simple aggregate of the results.
- you can not use reflection.
- returned final stats results must be immutable.
- again, keep in mind that the list of statistics is much longer than in the example.

Goals:

> If you add a new statistic, how many places in the code you need to change? Can it be just one?
> Can you add a new statistic calculation and force the developer to apply it at all right places?

⚠️ Important note: the purpose of this example is NOT to calculate the results. Yes, you will probably use DB, or whatever. The focus here is the _capabilities of the programming languages_.

Read the [../Guidelines.md](../Guidelines.md) for more information about coding style, naming, etc.

## Solutions

⭐️ [java-sad](java-sad) (by @igr, improvements by @bojanantonovic 🚀)

> Solution in Java. We can encapsulate individual rules (predicate with the counter).
  However, adding a new rule does not force the developer to apply it at all places.
  There are 3 actions developer have to modify on adding a new rule. The one with the
  highest cognitive load is the `ToyStats` class. The developer has to carefully follow the
  order of the attributes and map the values to the correct attributes.

⭐️ [haskell](haskell) (by @igr)

> Haskell solution. Immutable and pure. It directly updates the stats (i.e. creates a new state from    previous). Quite compact and elegant. Added `newtype`s for better type safety - you can't mix up the order of the attributes. We could use lenses for easier updates, but it's not necessary for this example.