-- https://cs.anu.edu.au/courses/comp1100/labs/06/
-- For tutorial only, Copyright reserved.

module Area where
import Data.Maybe

-- Exercise 1
-- data Maybe a = Nothing | Just a
-- https://www.haskell.org/hugs/pages/libraries/base/Data-Maybe.html
unmaybe :: [Maybe a] -> [a]
unmaybe list = case list of
    [] -> []
    x:xs -> maybeToList x ++ unmaybe xs

-- Exercise 2
join :: [a] -> [a] -> [a]
join xs [] = xs
join [] ys = ys
join (x:xs) ys = x : join xs ys

-- Exercise 3
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

-- Exercise 4
riffle :: [a] -> [a] -> [a]
riffle xs [] = xs
riffle [] ys = ys
riffle (x:xs) (y:ys) = x : y : riffle xs ys

-- Exercise 5
-- https://wiki.haskell.org/Performance/Accumulating_parameter
-- tail recursive
fastrev :: [a] -> [a]
fastrev n = fastrevHelper n []
    where
    fastrevHelper :: [a] -> [a] -> [a]
    fastrevHelper [] result = result
    fastrevHelper (x:xs) result = fastrevHelper xs (x:result)

-- Exercise 6
-- https://wiki.haskell.org/Peano_numbers
data Nat = Z | S Nat
    deriving (Show)

isOne :: Nat -> Bool
isOne n = case n of
    Z     -> False
    (S Z) -> True
    (S _) -> False

increment :: Nat -> Nat
increment n = (S n)

decrement :: Nat -> Nat
decrement n = case n of
    Z   -> error "predecessor: Zero has no predecessor"
    S m -> m

-- Exercise 6
natEq :: Nat -> Nat -> Bool
natEq Z Z = True
natEq x Z = False
natEq Z y = False
natEq (S x) (S y) = natEq x y

-- Exercise 7
addNat :: Nat -> Nat -> Nat
addNat Z y = y
addNat (S x) y = S (addNat x y)

-- Exercise 8
isNatEven :: Nat -> Bool
isNatEven Z = True
isNatEven (S Z) = False
isNatEven (S (S x)) = isNatEven x

-- Exercise 9
natToInt :: Nat -> Integer
natToInt Z = 0
natToInt (S x) = 1 + natToInt x

intToNat :: Integer -> Nat
intToNat 0 = Z
intToNat x = S (intToNat (x-1))

-- Exercise 10

-- Extension 1
-- https://mail.haskell.org/pipermail/haskell-cafe/2003-June/004484.html
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)


-- Extension 2


