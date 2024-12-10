module Maths.Prime where

  import Data.List as L

  -- Takes an int and returns a Bool if n is prime
  -- n must be > than 0 for to be valid
  isPrime :: Int -> Bool
  isPrime n = not (any (\m -> mod n m == 0) [2.. sqrtT n])

  sqrtT :: Int -> Int
  sqrtT = floor . sqrt . fromIntegral

  -- Determine whether two positive integer numbers are coprime.
  -- Two integers are coprime if their GCD is 1
  isCoprime :: Int -> Int -> Bool
  isCoprime a b = gcd a b == 1

  -- Calculate Euler’s totient function ϕ(m). Euler’s so-called totient function 
  -- ϕ(m) is defined as the number of positive integers r(1<=r<=m) that are coprime to m.
  totient :: Int -> Int
  totient m = length (filter (`isCoprime` m) [1..m])

  -- A list of the prime factors of a given positive integer.
  -- By default, the list should construct in ascending order
  primeFactors :: Int -> [Int]
  primeFactors n = let divs   = filter (\k -> mod n k == 0) [2..n]
                       factor = if null divs then 0 else head divs
                   in if factor == 0 then [] else factor : primeFactors (div n factor)

  -- Given an Int, returns a list of tuples containing its prime factors and their multiplicity
  --
  -- ghci> primeFactorsMultiplicity 100
  -- ghci> [(2, 5), (5, 2)]
  primeFactorsMultiplicity :: Int -> [(Int, Int)]
  primeFactorsMultiplicity n = map (\facs -> (head facs, length facs)) (L.group (primeFactors n))

  -- Goldbach’s conjecture.
  -- Goldbach’s conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
  -- E.g. 28 is the sum of 23 and 5, which are both prime
  goldbachNumbers :: Int -> [Int]
  goldbachNumbers n = let pairs = filter (\m -> isPrime m && isPrime (n - m)) [2..n]
                          m = if null pairs then 0 else m
                      in if m == 0 then [] else [m, n - m]

  -- A list of Goldbach compositions.
  -- Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
  goldbachCompositions :: Int -> Int -> [(Int, [Int])]
  goldbachCompositions low high = let lo = if even low then low else succ low
                                      hi = if even high then high else pred high
                                  in map (\n -> (n, goldbachNumbers n)) [lo,lo+2..hi]