{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Term (Term, MTerm (..), isZero, Term.divides, tLcm) where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Monomial (IntMonomial (..), ListMonomial (..), Monomial, divides, exponsAsList, mLcm)
import Numeric (floatToDigits)

class (Eq t, Ord t, Num t, Fractional t) => Term t where
  -- Returns True if the coefficient is zero, False otherwise.
  isZero :: t -> Bool

  {-
    Checks if the first term is divisible by the second term.
  -}
  divides :: t -> t -> Bool

  {-
    Gets the least common multiple of two terms
  -}
  tLcm :: t -> t -> t

data MTerm f m = MT f m

instance (Eq f, Eq m) => Eq (MTerm f m) where
  -- (==) :: MT l => l -> l -> Bool
  (==) (MT f1 m1s) (MT f2 m2s) = m1s == m2s

instance (Eq f, Monomial m) => Ord (MTerm f m) where
  -- (>) :: MT l => l -> l -> Bool
  (>) (MT f1 m1) (MT f2 m2) = m1 > m2

  -- (<) :: MT l => l -> l -> Bool
  (<) (MT f1 m1) (MT f2 m2) = m1 < m2

  -- (>=) :: MT l => l -> l -> Bool
  (>=) (MT f1 m1) (MT f2 m2) = (>) m1 m2 || (==) m1 m2

  -- (<=) :: MT l => l -> l -> Bool
  (<=) (MT f1 m1) (MT f2 m2) = (<) m1 m2 || (==) m1 m2

instance (Num f, Monomial m, Eq m) => Num (MTerm f m) where
  -- (+) :: MT l => l -> l -> l
  (+) (MT f1 m1) (MT f2 m2)
    | m1 /= m2 = error "Addition not defined for Terms of differing exponents."
    | otherwise = MT (f1 + f2) m1

  -- (-) :: MT l => l -> l -> l
  (-) (MT f1 m1) (MT f2 m2)
    | m1 /= m2 = error "Subtraction not defined for Monomials of differing exponents."
    | otherwise = MT (f1 - f2) m1

  -- (*) :: MT l => l -> l -> l
  (*) (MT f1 m1) (MT f2 m2) = MT (f1 * f2) (m1 * m2)

  -- (negate) :: MT l => l -> l
  negate (MT f m) = MT (negate f) m

  -- fromInteger :: MT l => l -> l
  fromInteger _ = error "Not implemented."

  -- abs :: MT l => l -> l
  abs (MT f m) = error "Cannot determine abs for term."

  -- signum :: MT l => l -> l
  signum (MT f m) = error "Cannot determine signum for term."

instance (Fractional f, Eq f, Show f, Monomial m, Fractional m, Show m) => Fractional (MTerm f m) where
  -- (/) :: MT l => l -> l -> l
  (/) (MT f1 m1) (MT f2 m2)
    | m2 `Monomial.divides` m1 = MT (f1 / f2) (m1 / m2)
    | otherwise = error ("Cannot divide the term " ++ show (MT f1 m1) ++ " with " ++ show (MT f2 m2))

instance (Show f, Show m, Monomial m, Eq f, Num f, Fractional f) => Show (MTerm f m) where
  -- show :: ListMonomial lm => lm -> String
  show (MT f m)
    | all (== 0) (exponsAsList m) = show f
    | otherwise = (if f == 1 then "" else show f ++ " ") ++ show m

formatFloat :: Float -> String
formatFloat 1.0 = "1"
formatFloat f
  | f < 0 = "(-" ++ formatFloat (abs f) ++ ")"
  | otherwise = if (length . fst) floatT == snd floatT then show (round f) else show f
  where
    floatT = floatToDigits 10 f

instance (Eq f, Fractional f, Ord f, Monomial m, Fractional m, Show f, Show m) => Term (MTerm f m) where
  -- isZero :: Term t => t -> Bool
  isZero (MT f m)
    | f == 0 = True
    | otherwise = False

  divides (MT _ m1) (MT _ m2) = m1 `Monomial.divides` m2

  tLcm (MT f1 m1) (MT f2 m2) = MT (maximumBy (comparing abs) [f1, f2]) (mLcm m1 m2)
