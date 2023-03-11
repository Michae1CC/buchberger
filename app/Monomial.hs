{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Monomial (Monomial, ListMonomial (..), IntMonomial (..), divides, exponsAsList, mLcm) where

import Data.Bits
import Data.List
import Data.Word

{-
    A class definition for a Monomial. A Monomial is a polynomial in which all
    coefficients are 0 expect for one term.

    Must support Monomial equality. Example:
      x^2 == x^2 = True
      3x^2 == x^2 = False
      y^2 == x^2 = False

    Must support Graded Monomial ordering on variable exponents. We say
        \alpha x_{1}^{a_{1}} x_{2}^{a_{2}} ... x_{n}^{a_{n}} >= or >
        \beta  x_{1}^{b_{1}} x_{2}^{b_{2}} ... x_{n}^{b_{n}}
    when
      \sum_{i} a_{i} >= or > \sum_{i} b_{i}
    . Examples:
        2 x^3 y^2 z^0 > 3 x^2 y^4 z^5 = False
        7 x^1 y^5 z^2 > 3 x^2 y^1 z^3 = True
        7 x^1 y^5 z^2 < 3 x^2 y^1 z^3 = False

    Must support polynomial addition, subtraction and multiplication. Examples
      3x^2 + 2x^2 = 5x^2
      3x^2 - 2x^2 = x^2
      3 x^2 y^3 * xz = 3 x^3 y^3 z

    Must support polynomial quotients and remainders. Examples:
      6 x^2 y^3 / 2xz = (3 x y^3, z)
      2 x^2 y^3 / 6xz = (x y^3, 3z)
-}
class (Eq m, Ord m, Num m) => Monomial m where
  {-
    Extracts the coefficient of the Monomial.
  -}
  -- coeff :: (Show f, Num f, Integral f, Eq f) => m -> f

  {-
    Returns the exponents of the Monomial as an ordered list.
  -}
  exponsAsList :: (Num a) => m -> [a]

  {-
    Checks if the monomial m1 is divisible by the monomial m2.
  -}
  divides :: m -> m -> Bool
  divides m2 m1 = all (>= 0) (zipWith (-) v1s v2s)
    where
      v1s = exponsAsList m1
      v2s = exponsAsList m2

  {-
    Gets the least common multiple of two monomials
  -}
  mLcm :: m -> m -> m

  {-
      Lexicographical Monomial ordering on variable exponents. We say
          \alpha x_{1}^{a_{1}} x_{2}^{a_{2}} ... x_{n}^{a_{n}} >=
          \beta  x_{1}^{b_{1}} x_{2}^{b_{2}} ... x_{n}^{b_{n}}
      when for the first (from the left) non-zero (a_{i} - b_{i}) is
      non-negative. Examples:
          2 x^3 y^2 z^0 > 3 x^2 y^4 z^5 = True
          7 x^1 y^5 z^2 > 3 x^2 y^1 z^3 = False
          7 x^1 y^5 z^2 < 3 x^2 y^1 z^3 = True
  -}
  (>>>) :: m -> m -> Bool
  (<<<) :: m -> m -> Bool
  (>>>=) :: m -> m -> Bool
  (<<<=) :: m -> m -> Bool

  (>>>) m1 m2 = v1s > v2s
    where
      v1s = exponsAsList m1
      v2s = exponsAsList m2

  (<<<) m1 m2 = not ((Monomial.>>>) m1 m2)
  (>>>=) m1 m2 = (Monomial.>>>) m1 m2 || (==) m1 m2
  (<<<=) m1 m2 = (Monomial.<<<) m1 m2 || (==) m1 m2

{-
    Represents the variables of a Monomial as a haskell List data structure.
    The f stands for the coefficient of the monomial
    over the field F. The second parameter represents the exponents of the
    variables of the Monomial. That is the first item of [v] is the
    exponent of the first variable, the second item the exponent of
    the second variable etc. As an example
        LM 3 [1,6,2]
    would be the ListMonomial representation of the Monomial
        3 x_{1}^{1} x_{2}^{6} x_{3}^{2}
-}
newtype ListMonomial v = LM [v]

showExp :: (Show c, Eq c, Num c) => String -> c -> String
showExp _ 0 = ""
showExp v 1 = v
showExp v c = v ++ "^" ++ show c

instance (Show v, Eq v, Num v) => Show (ListMonomial v) where
  -- show :: ListMonomial lm => lm -> String
  show (LM vs) = unwords $ filter (/= "") $ zipWith showExp vars vs
    where
      vars = if length vs <= 5 then ["x", "y", "z", "w", "v"] else map (\i -> "x_{" ++ show i ++ "}") [1 ..]

instance (Eq v, Num v) => Eq (ListMonomial v) where
  -- (==) :: ListMonomial lm => lm -> lm -> Bool
  (==) (LM v1s) (LM v2s) = mv1s == mv2s
    where
      maxLen = max (length v1s) (length v2s)
      mv1s = take maxLen (v1s ++ repeat 0)
      mv2s = take maxLen (v2s ++ repeat 0)

instance (Eq v, Ord v, Integral v) => Ord (ListMonomial v) where
  -- Lexicographic ordering.
  -- Typically denoted as >_{lex}

  -- (>) :: m -> m -> Bool
  (>) m1 m2 = v1s > v2s
    where
      v1s = exponsAsList m1
      v2s = exponsAsList m2

  -- (<) :: m -> m -> Bool
  (<) m1 m2 = v1s < v2s
    where
      v1s = exponsAsList m1
      v2s = exponsAsList m2

  -- (>=) :: m -> m -> Bool
  (>=) m1 m2 = (>) m1 m2 || (==) m1 m2

  -- (<=) :: m -> m -> Bool
  (<=) m1 m2 = (<) m1 m2 || (==) m1 m2

instance (Num v) => Num (ListMonomial v) where
  -- (+) :: ListMonomial lm => lm -> lm -> lm
  (+) l1 l2 = error "Addition not defined for Monomials."

  -- (+) :: ListMonomial lm => lm -> lm -> lm
  (-) l1 l2 = error "Subtraction not defined for Monomials."

  -- (*) :: ListMonomial lm => lm -> lm -> lm
  (*) (LM v1s) (LM v2s) = LM zvs
    where
      maxLen = max (length v1s) (length v2s)
      zvs = take maxLen $ zipWith (+) (v1s ++ repeat 1) (v2s ++ repeat 1)

instance (Num v, Show v, Integral v) => Fractional (ListMonomial v) where
  -- (/) :: ListMonomial lm => lm -> lm -> lm
  (/) (LM v1s) (LM v2s)
    | LM v2s `divides` LM v1s = LM (zipWith (-) v1s v2s)
    | otherwise = error ("Cannot divide the monomial " ++ show v2s ++ " with " ++ show v1s)

instance (Num v, Integral v, Eq v, Ord v) => Monomial (ListMonomial v) where
  -- exponsAsList :: lm => lm -> [Int]
  exponsAsList (LM vs) = map fromIntegral vs

  -- mLcm :: m -> m -> m
  mLcm (LM e1) (LM e2) = LM (zipWith max e1 e2)

  -- (>>>) :: ListMonomial lm => lm -> lm -> Bool
  (>>>) (LM v1s) (LM v2s) = sum v1s > sum v2s

{-
    Represents the variables of a Monomial as a haskell as a single 64bit
    unsigned haskell Word. The bits are arranged so that the first n 16 bits
    are dedicated to the exponent of the nth variable. As an example
      IM 3 281500746645504
    would be the IntMonomial representation of the Monomial
      3 x_{1}^{1} x_{2}^{6} x_{3}^{2} x_{4}^{0}
    since if we viewing the 16bit chunks of 281500746645504:
      0000000000000001 0000000000000110 0000000000000010 0000000000000000
    we find the binary representation is found in the first 16bits which
    corresponds to the exponent of 1 for our first variable with analogous
    reasoning for the remaining 16bit chunks.

    NOTE: For this implementation of the monomial we are restricted to a
    maximum of 4 variables and exponents are limited to integers within
    the range [0, 2 ** 16 -1].
-}
newtype IntMonomial = IM Word64

instance Show IntMonomial where
  -- show :: ListMonomial im => im -> String
  show (IM v) = unwords $ filter (/= "") $ zipWith showExp ["x", "y", "z", "w"] vs
    where
      vs = reverse $ map ((.&.) (shift 1 16 Prelude.- 1) . shift v) [0, -16 .. -48]

instance Eq IntMonomial where
  -- (==) :: IntMonomial im => im -> im -> Bool
  (==) (IM n1) (IM n2) = n1 Prelude.== n2

instance Ord IntMonomial where
  -- (>) :: m -> m -> Bool
  (>) m1 m2
    | sum v1s == sum v2s = v1s > v2s
    | otherwise = sum v1s > sum v2s
    where
      v1s = exponsAsList m1
      v2s = exponsAsList m2

  -- (<) :: m -> m -> Bool
  (<) m1 m2
    | sum v1s == sum v2s = v1s < v2s
    | otherwise = sum v1s < sum v2s
    where
      v1s = exponsAsList m1
      v2s = exponsAsList m2

  -- (>=) :: m -> m -> Bool
  (>=) m1 m2 = (>) m1 m2 || (==) m1 m2

  -- (<=) :: m -> m -> Bool
  (<=) m1 m2 = (<) m1 m2 || (==) m1 m2

instance Num IntMonomial where
  -- (+) :: IntMonomial im => im -> im -> im
  (+) l1 l2 = error "Addition not defined for Monomials."

  -- (+) :: IntMonomial im => im -> im -> im
  (-) l1 l2 = error "Subtraction not defined for Monomials."

  -- (*) :: IntMonomial im => im -> im -> im
  (*) (IM n1) (IM n2) = IM (n1 + n2)

instance Monomial IntMonomial where
  -- (+) :: ListMonomial im => im -> [Int]
  exponsAsList (IM n1) = reverse $ map (fromIntegral . (.&.) (shift 1 16 Prelude.- 1) . shift n1) [0, -16 .. -48]
