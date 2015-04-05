-- | First assignment for IB016, semester spring 2015
-- Name: Jan Skrasek
-- UID: 373816

module Polynomial (
    -- * Polynomial type
      Polynomial (..)
    -- * Printing
    , polyPrint
    -- * Evaluation
    , eval
    , belongs
    -- * Properties
    , yIntersect
    , quadratic
    -- * Operations
    , scalarTimes
    , polyPlus
    , polyMinus
    , polyTimes
    ) where

import Data.List ()

-- | The Polynomial type. Polynomials are represented as a list of coefficients
-- of increasing power. That is, the first element of list is constant element,
-- the second is the coefficient for x, the third coefficient for x^2 and so on.
-- There should be no trailing zeroes in the polynomial representation, i.e.
-- x^2 + 2x + 3 is represented as @'P' [3,2,1]@ not @'P' [3,2,1,0,0]@ or similar.
newtype Polynomial a = P { unP :: [a] } deriving Show

-- | Evaluate polynomial in given value. This can be viewed as
-- converting the polynomial to the evaluation function.
--
-- >>> eval (P [3, 1, -5]) 2
-- -15
eval :: (Num a) => Polynomial a -> (a -> a)
eval p x = polySum (unP p) x 0

polySum :: (Num a) => [a] -> a -> Int -> a
polySum [] _ _     = 0
polySum (c:cs) x i = (c * (x ^ i)) + polySum cs x (i + 1)



-- | Determines, if a given point belongs to the polynomial.
--
-- >>> belongs (2, -15) (P [3, 1, -5])
-- True
belongs :: (Num a, Eq a) => (a, a) -> Polynomial a -> Bool
belongs (a, b) c = eval c a == b

-- | Determines, if the polynomial is quadratic
--
-- >>> quadratic (P [1, 2, -1])
-- True
quadratic :: (Num a, Eq a) => Polynomial a -> Bool
quadratic p = length (normalize (unP p)) == 3

-- | Find the coordinates of the polynomial intersecting the y axis.
--
-- >>> yIntersect (P [1, 2, -1])
-- (0,1)
yIntersect :: Num a => Polynomial a -> (a, a)
yIntersect p = (0,eval p 0)

-- | Print polynomial into pretty string representation.
--
-- The first argument is the variable name. Write spaces in between the term
-- operators. Coefficients 1 and -1 should be diplayed only as a sign symbol.
-- Do not display terms with zero coefficients.
--
-- >>> polyPrint "x" (P [1, 2, 3, 4])
-- "4x^3 + 3x^2 + 2x + 1"
-- >>> polyPrint "x" (P [-1, 0, -1, 3, -4])
-- "- 4x^4 + 3x^3 - x^2 - 1"
-- >>> polyPrint "x" (P [0, 0, -1])
-- "- x^2"
-- >>> polyPrint "x" (P [])
-- "0"
polyPrint :: (Num a, Ord a, Show a) => String -> Polynomial a -> String
polyPrint var p = listPrint var (unP p)

listPrint :: (Num a, Ord a, Show a) => String -> [a] -> String
listPrint _   [] = "0"
listPrint var l  = listPrintS . reverse $ zip l (map (\(n, e) -> memberPrint var (abs n) e) (zip l [0..]))

listPrintS :: (Num a, Ord a, Show a) => [(a,String)] -> String
listPrintS [] = ""
listPrintS ((0,_):xs) = "" ++ listPrintS xs
listPrintS ((n,e):xs) = (if n < 0 then "- " else "") ++ e ++ listPrintR xs

listPrintR :: (Num a, Ord a, Show a) => [(a,String)] -> String
listPrintR [] = ""
listPrintR ((0,_):xs) = "" ++ listPrintR xs
listPrintR ((n,e):xs) = (if n < 0 then " - " else " + ") ++ e ++ listPrintR xs

memberPrint :: (Num a, Ord a, Show a) => String -> a -> Int -> String
memberPrint _   0 _ = ""
memberPrint _   n 0 = show n
memberPrint var 1 1 = var
memberPrint var n 1 = show n ++ var
memberPrint var 1 e = var ++ "^" ++ show e
memberPrint var n e = show n ++ var ++ "^" ++ show e


-- | Multiply scalar and polynomial.
--
-- >>> scalarTimes 3 (P [1, 2, 3])
-- P [3, 6, 9]
-- >>> scalarTimes 0 (P [1, 2, 3])
-- P []
scalarTimes :: (Eq a, Num a) => a -> Polynomial a -> Polynomial a
scalarTimes c p = P . normalize $ map (*c) (unP p)

normalize :: (Eq a, Num a) => [a] -> [a]
normalize a = reverse . normalize_ $ reverse a

normalize_ :: (Eq a, Num a) => [a] -> [a]
normalize_ [] = []
normalize_ (c:xc) = if c == 0 then normalize_ xc else c : xc


-- | Sum two polynomials.
--
-- >>> polyPlus (P [1, 2, 3]) (P [2, 2, -3])
-- P [3, 4]
polyPlus :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
polyPlus p1 p2 = P . normalize $ listPlus (unP p1) (unP p2)

listPlus :: (Num a, Eq a) => [a] -> [a] -> [a]
listPlus = myZipWith (+)

myZipWith :: (Num a, Eq a) => (a -> a -> a) -> [a] -> [a] -> [a]
myZipWith _  [] [] = []
myZipWith fn []     (b:lb) =  fn 0 b : myZipWith fn [] lb
myZipWith fn (a:la) []     = fn a 0 : myZipWith fn la []
myZipWith fn (a:la) (b:lb) = fn a b : myZipWith fn la lb



-- | Make a difference of two polynomials.
--
-- >>> polyMinus (P [2, -5, 3, 3]) (P [1, 2, 3])
-- P [1, -7, 0, 3]
polyMinus :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
polyMinus p1 p2 = P . normalize $ myZipWith (-) (unP p1) (unP p2)


-- | Multiply two polynomials.
--
-- >>> polyTimes (P [2, 1]) (P [2, 3])
-- P [4, 8, 3]
polyTimes :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
polyTimes p1 p2 = P . normalize $ listTimes 0 (unP p1) (unP p2)

listTimes :: (Num a, Eq a) => Int -> [a] -> [a] -> [a]
listTimes _ [] _  = []
listTimes n (c:p1) p2 = listPlus (replicate n 0 ++ map (*c) p2) (listTimes (n + 1) p1 p2)



