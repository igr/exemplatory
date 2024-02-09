# StopWatch ⏱️

Your task is to model a stop watch. It has the following _capabilities_:

+ `start` - starts the stop watch.
+ `stop` - stops the running stop watch.
+ `elapsed` - provides information about the elapsed time.

Example: measure 1s thread sleep.

Read the [../Guidelines.md](../Guidelines.md) for more information about coding style, naming, etc.

## Solutions

⭐️ [java-mutable](java-mutable) (by @igr)

> Probably a go-to implementation that anyone with OOP background would write. Also, a BAD and BAD one! The state is mutable and methods are impure. There is nothing preventing users from misusing the class, e.g. calling `elapsed()` on non-started watch.

⭐️ [java-oop](java-oop) (by @igr)

> Much better solution! The state is encapsulated. The class is immutable. The code itself direct the usage - there is no way to mis-use the class. But... methods are not pure, since they are using `System.currentTimeMillis()`.

⭐️ [kt-types](kt-types) (by @igr)

> Solution with data types only: no functions or methods whatsoever! Interesting: this approach is focused on state snapshots, and not operations (methods or functions). Uses ADT (algebraic data types) to model state transitions - although may not be necessary in this case.

⭐️ [haskell](haskell) (by @igr)

> And now, the same solution in Haskell. The code is much more concise, and the state transitions are modeled using pattern matching. The code is not pure (since the IO effect), and the state is immutable. The code is also very readable and easy to understand.

⭐️ [haskell-mtl](haskell-mtl) (by @igr)

> This time, IO is abstracted using the `mtl` library. The implementation is defined at use; however, it would make an orphan instance. Instead, new wrapper type is created with the `newtype`.

⭐️ [haskell-2](haskell-2) (by help from Reddit)

> Better approach than previous.


🧧 Contribute.
