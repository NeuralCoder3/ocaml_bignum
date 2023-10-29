
# Bignumbers

<!-- What -->
This library contains a small and self-contained implementation of unbounded  (arbitrary precision) integers, also known as BigNum or BigInt.
The library does not use any other libraries and is usuable by copying it into any other OCaml code.
For this purpose, the main modules of the library are not separated into individual files.

## Why

Oftentimes, one wants unbounded numbers to play around, demonstrate interesting facts or just free oneself from the harsh reality of physical machines.

Some languages like SML (or more precisely many implementations of SML) offer
them out of the box.

There are many libraries for OCaml which implement these numbers.
Many of them far superior in scope, speed, and usability.
Some were even part of the OCaml core in previous versions.
However, most of them need to be installed and are not easy to 
inspect for untrained users.

With this library, we try to provide a short, self-contained library to be used
everywhere and be looked at by everyone with a basic understanding of OCaml.

In the end, the code got a bit longer than expected (140 for naturals + 64 for an extension to Z with all functions and formatting).
But it is still quite short and manageable for embedding.

## Representation

We define a common bignum interface for numbers in general.
This interface is implemented for the natural numbers and integers.
Natural numbers are implemented directly.
Integers are implemented on top of natural numbers.

### Natural Numbers

We represent natural numbers as a list of bits in reversed order.
The head of the list is the least significant bit (LSB).
The last element is the most significant bit (MSB).
The n-th element represents the value $2^n$.

Each bit is represented by a `bool`.
The empty list represents zero.
Appending `false` to a number does not change its value.
Therefore, the representation is not unique.

This representation is suboptimal in many regards.
It was chosen to be understandable to a broad audience.
Alternatives are discussed below.

The reversed nature of the list allows easier implementation of most operations.

### Integers

The integers are implemented as a functor acting on an existing bignum implementation.
The assumption is that a natural number implementation is provided and then extended to integers.

An integer is represented by a sign together with a natural number.
The sign is implemented using sum types to avoid boolean blindness.

Additional to the ambiguity of the underlying natural numbers, zero
has two encodings (positive and negative).

### Notations

We present a functor on a bignum implementation that overwrites the usual integer operations with the custom ones.
Additionally, the functions `i2b`, `b2i`, `s2b`, and `b2s` are defined as aliases to convert from and to integers and strings from big numbers.
(Note: `s2b` expects a binary string, `b2s` produces a decimal string for readability.)

## Runtime Analysis

We present the functions for natural numbers and their asymptotic running time:
| Function         | Running time in O() |
| ---------------- | ------------------- |
| is_zero          | $\log n$         |
| add              | $\log n + \log m$ |
| sub              | $\log n + \log m$ |
| mul              | $\log n \cdot (2\cdot \log n + \log m) = \log^2 n$ |
| cmp              | $\log n + \log m$ |
| divmod           | $\log n + \log n \cdot (\log n + \log m + \log n + \log m) = \log^2 n$ |
| string_of_bignum | $\log n \cdot \log^2 n = \log^3 n$ |
| abs, shr, shl    | $1$ |
| pow              | $\log n \cdot \log^2 n = \log^3 n$ |

The input length $n$ is the represented integer itself.
Therefore, $\log n$ is the size of the number representation.
If two numbers are involved, $m$ is the represented second number.
Note: We use $\mathcal{O}(\max(a,b)) = \mathcal{O}(a+b)$ and $\mathcal{O}(\log_a x) = \mathcal{O}(\log_b x)$.

From the theory, the division is the most challenging operation.

For integers, the extension of all functions has a negligible overhead.

## Alternative Ways

There are many ways to implement the functions or the whole library better, faster, stronger.

As all functions operate on binary numbers, most circuit
implementations can be leveraged to a working (and fast) implementation.
In this setting, especially the addition and subtraction function would change.
One could easily exchange the recursive case with the usual circuit logic.
This might be a bit faster. We opted for the current implementation for readability.

In this context, there are also many faster algorithms to choose.
One could implement the multiplication for instance using the Karatsuba algorithm.

### Representations
The binary implementation is a straight forward one.
It is quite simple (we only have bits 0 and 1) and
it is fast (logarithmic size).

One very simple but only theoretically interesting implementation is the unary encoding as:
```
type N = O | S of N
```
The encoding becomes infeasible even for small numbers and nearly all operations become prohibitively expensive.

An improvement of the current encoding would be a generalization to a n-ary system.
Asymptotically, nothing would change.
However, the constants would be much smaller.
The needed storage would also be more efficient (especially as booleans are usually stored wastefully as bytes or even ints).

In an extreme case, the base `pow 2 (Sys.int_size)` could be chosen.
We could leverage normal integer operations for each part.
The overhead in the normal integer space would be nearly zero.
The recursion depth (and all constants) would be only a 32th of the current allowing for much larger numbers.

## Tests

You can run the tests using `dune test`.
The tests generate 100000 random numbers for each function and compare
the result of the bignum library with OCaml's built-in functions.
To do so, the inputs are converted to big numbers and the results are converted back.

## Caveats & TODO

The implementation is not perfect (by design).

The main problems with the library are:
- conversion to decimal string is too slow
- stack overflow due to deeply nested (non-tail-) recursion

Here are some possible improvements:
- Make the functions tail-recursive (harder to read, need CPS)
- Generalize the representation as explained above (harder to understand)

## Resources

- [Karatsuba algorithm for multiplication](https://en.wikipedia.org/wiki/Karatsuba_algorithm)
- [Pseudocode for binary divmod (and general division algorithms)](https://en.wikipedia.org/wiki/Division_algorithm#Integer_division_(unsigned)_with_remainder)
- [binary number algorithms](https://en.wikipedia.org/wiki/Binary_number#Division)
- [long division](https://en.wikipedia.org/wiki/Long_division)
- OCaml bignum implementations:
    - [ocaml/num](https://github.com/ocaml/num)
    - [janestreet/bignum](https://github.com/janestreet/bignum)
    - [ocaml/zarith](https://github.com/ocaml/Zarith)