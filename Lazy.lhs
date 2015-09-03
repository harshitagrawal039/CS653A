Haskell supports lazy evaluations. This means that no expression is
evaluated unless it is needed. This module demonstrates some examples
of such code.

> module Lazy where
>

An infinite repetition of a given element.

> repeat :: a -> [a]
> repeat a = z
>    where z = a:z

Now comes the Fibonacci series as an infinite list

> fib :: [Integer]
> fib = 1 : 1 : rest
>   where rest = zipWith (+) fib (tail fib)

The primes computed using the sieve of Eratosthenes.

> primes :: [Integer]
> primes = sieve [2..]
>   where sieve (x:xs) = x : sieve ( filter (not . multipleOf x) xs )
>         sieve _      = []
>         multipleOf x y = y `mod` x == 0
