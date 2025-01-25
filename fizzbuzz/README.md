# FizzBuzz 🪅

## Rule Set 1

1. The algorithm must work at least for integers in the range `1`-`1000`. We do not care for numbers outside that range.
2. Multiples of `3` make `Fizz`.
3. Multiples of `5` make `Buzz`.
4. Multiples of `3` and `5` make `FizzBuzz`.
5. FizzBuzz is mission-critical code. Bad inputs should not affect the output instead of catastrophically failing.
6. Rules are always valid unless overridden by a new rule.

My assumptions:

+ The result of the non-multiples of `3` and `5` is the number itself.

## Rule Set 2

1. Max of `30` lines. Lines taken by the input array do not count.
2. Max width of `100` characters. Lines must break on natural breakpoints and not with the aim of optimizing for following the rules. Comments do not count toward this limit and would be positively valued if they are helpful.
3. Variables cannot have one or two letters name unless the name is meaningful, the variable meaning can be inferred from its usage or where it is common to do so within the language (e.g: loops)
4. Readability: indentation or newlines cannot be omitted.
5. The result must be represented by a string. Bad inputs should return an empty string.
6. Mutating input operations are forbidden.

## Rule Set 3

1. Numeric types, number literals and their associated methods and operations are forbidden. The input array must contain a **string** representations of the numbers. The programmer can use whatever representation they see fit with the only restriction being that it could only contain _letters_, _numbers_ and _symbols_ that could be typed with a single stroke (on an US keyboard). The max length of a string representing a number is `6` characters.

## Rule Set 4

1. Input may contain 0. If it is not possible to represent 0 due to the chosen numeric representation the representation must be changed to allow for it.

My assumptions:

+ The `0` returns `0`.

## Rule Set 5

1. The input array can contain invalid representations of numbers mixed with valid representations. Invalid representations must not affect the output of the program.

## Rule Set 6

1. Negative numbers are allowed in the input array, they should be ignored.

## Rule Set 7

1. If the language and the number representation allows it, the input can contain decimals. If the number representation does not allow decimal numbers, it must be changed so that they can be representable.
2. Decimal `0` (`0.0`) and negative decimal zero (`-0.0`) must have their own representation and be treated as decimal numbers (produce the empty string).

My assumption:
+ decimal numbers are ignored.