{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Polynomial (Polynomial) where

import Data.List
import Data.Ord (comparing)
import Monomial (IntMonomial (..), ListMonomial (..), Monomial)
import Term (MTerm (..), Term, divides, isZero, tLcm)

{-
    A class definition for a Polynomial. A Polynomial is a finite linear
    combination of Monomials of the form.

    Must support polynomial addition, subtraction and multiplication. Examples
      (3x^2 + 2y^2) + (z + y^2) = 3x^2 + 3y^2 + z
      (3x^2 + 2y^2) - y^2 = 3x^2 + y^2
      (3x^2 + 2y^2) * 2 y^2 = 6 y^2 x^2 + 4 y^4

    Must support polynomial division. Examples:
      (x^5 z^2 + x^4 y + x^2 y^2 z + x^2 z^2 + y^2) / (x^2 z + 1) = x^3 z + y^2 + z
-}
class (Num p) => Polynomial p

newtype ListPolynomial t = LP [t]

mulT :: (Term t) => [t] -> t -> [t]
mulT fs x = filter (not . isZero) $ map (x *) fs

mergeWith :: (Term t) => (t -> t -> t) -> [t] -> [t] -> [t]
mergeWith m fs [] = fs
mergeWith m [] gs = gs
mergeWith m (f : fs) (g : gs)
  | f > g = f : mergeWith m fs (g : gs)
  | f < g = g : mergeWith m (f : fs) gs
  | otherwise = if isZero merged then mergeWith m fs gs else merged : mergeWith m fs gs
  where
    merged = m f g

fromList :: (Term t) => [t] -> [t]
fromList l = reverse $ map (foldl1 (+)) ((group . sort) l)

{-
  A helper function for the division algorithm. Takes in parameters (in order):
    f g q r p
  and performs one step of the division algorithm.
-}
hDiv :: (Term t) => [t] -> [t] -> [t] -> [t] -> [t] -> ([t], [t])
hDiv f [] _ _ _ = ([], f)
hDiv _ _ q r [] = (q, r)
hDiv f (ltg : g) q r (ltp : p)
  | ltg `Term.divides` ltp = hDiv f (ltg : g) (mergeWith (+) [t] q) r (mergeWith (+) (ltp : p) $ map negate (mulT (ltg : g) t))
  | otherwise = hDiv f (ltg : g) q (mergeWith (+) r [ltp]) p
  where
    t = ltp / ltg

pDiv :: (Term t) => [t] -> [t] -> ([t], [t])
pDiv f g = hDiv f g [] [] f

{-
  A function used to filter which polynomials from a set Q can be used to
  reduce a polynomial p.
-}
fReduce :: (Term t) => [t] -> [[t]] -> [[t]]
fReduce ps qss = filter (canReduce ps) qss
  where
    canReduce _ [] = False
    canReduce [] _ = False
    canReduce (ltp : ps) (ltq : qs) = ltq `Term.divides` ltp

{-
  A helper function that checks if a polynomial p can be reduced so that it
  has no remainder. Takes in parameters
    p Q
  where p is the polynomial to reduce and Q is a list of polynomials with
  which to reduce p by.
-}
canReduce :: (Term t) => [t] -> [[t]] -> Bool
canReduce [] _ = True
canReduce _ [] = False
canReduce p qs
  | null rs = False
  | otherwise = or [canReduce (reduceBy p r) qs | r <- rs]
  where
    rs = fReduce p qs

reduceBy :: (Term t) => [t] -> [t] -> [t]
reduceBy [] _ = []
reduceBy p [] = p
reduceBy (ltp : p) (ltr : r) = mergeWith (+) (ltp : p) $ map negate (mulT (ltr : r) t)
  where
    t = ltp / ltr

reduce2 :: (Term t) => [t] -> [[t]] -> [t]
reduce2 [] _ = []
reduce2 p [] = p
reduce2 p qs
  | canReduce p qs = []
  | null rs = p
  | otherwise = minimum [reduce2 (reduceBy p r) qs | r <- rs]
  where
    rs = fReduce p qs

reduce :: (Term t) => [t] -> [[t]] -> [t]
reduce [] _ = []
reduce p [] = p
reduce [p0] qs
  | canReduce [p0] qs = []
  | otherwise = [p0]
reduce (p : ps) qs
  | canReduce (p : ps) qs = []
  | null rs = p : reduce ps qs
  | otherwise = minimum [reduce (reduceBy (p : ps) r) qs | r <- rs]
  where
    rs = fReduce (p : ps) qs

sPoly :: (Term t) => [t] -> [t] -> [t]
sPoly p q = mergeWith (+) mp (map negate mq)
  where
    lcmMpMq = tLcm (head p) (head q)
    lcmMpMqDivp = lcmMpMq / (head p)
    lcmMpMqDivq = lcmMpMq / (head q)
    mp = mulT p lcmMpMqDivp
    mq = mulT q lcmMpMqDivq

selectPair :: [a] -> [(Int, Int)] -> (a, a, [(Int, Int)])
selectPair _ [] = error "Cannot select from empty element list"
selectPair es ks = (es !! i, es !! j, tks)
  where
    (i, j) = head ks
    tks = tail ks

hBucherberger :: (Term t) => [[t]] -> [(Int, Int)] -> [[t]]
hBucherberger gs ks
  | null ks = gs
  | otherwise = hBucherberger ngs nnks
  where
    (gi, gj, nks) = selectPair gs ks
    h = reduce (sPoly gi gj) gs
    ngs = if null h then gs else gs ++ [h]
    nnks = if null h then nks else nks ++ [(n, length gs) | n <- [0 .. (length gs - 1)]]

bucherberger :: (Term t) => [[t]] -> [[t]]
bucherberger gs = hBucherberger gs [(i, j) | i <- [0 .. l], j <- [0 .. l]]
  where
    l = length gs - 1

instance (Term t) => Num (ListPolynomial t) where
  -- (+) :: ListPolynomial p => p -> p -> p
  (+) (LP fs) (LP gs) = LP $ mergeWith (+) fs gs

  -- (-) :: ListPolynomial p => p -> p -> p
  (-) (LP fs) (LP gs) = LP $ mergeWith (+) fs $ map negate gs

  -- (*) :: ListPolynomial p => p -> p -> p
  (*) (LP fs) (LP gs) = LP $ foldl1 (mergeWith (+)) [mulT gs fi | fi <- fs]

instance (Show t) => Show (ListPolynomial t) where
  -- show :: ListPolynomial p => p -> String
  show (LP fs)
    | null fs = "0"
    | otherwise = intercalate " + " (map show fs)

instance (Term t) => Polynomial (ListPolynomial t)
