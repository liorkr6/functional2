{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, mod, not, notElem, null, or, otherwise, product, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

-- Section 1: String matching
isPrefixOf :: String -> String -> Bool
isPrefixOf str1 str2 = str1 == take (length str1) str2

isInfixOf :: String -> String -> Bool
isInfixOf _ [] = False
isInfixOf infixStr (x : xs) = isPrefixOf infixStr (x:xs) || isInfixOf infixStr xs

isSuffixOf :: String -> String -> Bool
isSuffixOf suffixStr str = reverse suffixStr `isPrefixOf` reverse str

isSubseuenceOf :: String -> String -> Bool
isSubseuenceOf _ [] = False
isSubseuenceOf [] _ = True
isSubseuenceOf (x:xs) (y:ys) =
    if x == y then isSubseuenceOf xs ys else isSubseuenceOf (x:xs) ys
-- -- Section 2: Document searching
type Phrase = String
data Query = All [Query] | Any [Query] | None [Query] | Literal Phrase deriving Show
type Document = String

isDocumentMatch:: Document -> Query -> Bool
isDocumentMatch d q = case q of
  Literal phrase -> phrase `isInfixOf` d
  All queries -> all (isDocumentMatch d) queries
  Any queries -> any (isDocumentMatch d) queries
  None queries -> not (any (isDocumentMatch d) queries)

findDocuments :: Query -> [Document] -> ([Document], [Document])
findDocuments query = foldr match ([], [])
  where
    match doc (matches, nonMatches)
      | isDocumentMatch doc query  = (doc : matches, nonMatches)
      | otherwise = (matches, doc : nonMatches)

-- Section 3: InfiniteList
data InfiniteList a = a :> InfiniteList a
infixr 3 :>

itoList :: InfiniteList a -> [a]
itoList (x:>xs) = x : itoList xs

irepeat :: a -> InfiniteList a
irepeat x = x :> irepeat x

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

icycle :: [a] -> InfiniteList a
icycle list = foldr (:>) (icycle list) list

naturals :: InfiniteList Int
naturals = iiterate (+1) 0

sample :: InfiniteList a -> [a]
sample = take 10 . itoList

integers :: InfiniteList Int
integers = iiterate (\n -> if n > 0 then -n else -n+1) 0

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) =  f x :> imap f xs

iscan :: (a -> b -> b) -> b -> InfiniteList a -> InfiniteList b
iscan f x (y :> ys) = f y x :> iscan f (f y x) ys

izip :: InfiniteList a -> InfiniteList b -> InfiniteList (a, b)
izip (x :> xs) (y :> ys) = (x, y) :> izip xs ys

interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
interleave (x:>xs) (y:>ys)= x :> y :> interleave xs ys

iinits :: InfiniteList a -> InfiniteList [a]
iinits xs = imap (\n -> take n (itoList xs)) naturals

itails :: InfiniteList a -> InfiniteList (InfiniteList a)
itails (x :> xs) = (x :> xs) :> itails xs

-- -- Bonus: if you don't wish to implement this, simply write ifind = undefined
-- ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool

-- -- Section 4: Binary trees (no necessarily search trees)
data Tree a = EmptyTree | Tree (Tree a) a (Tree a) deriving Show
preOrder :: Tree a -> [a]
preOrder tree = case tree of
    EmptyTree 
-- postOrder :: Tree a -> [a]
-- inOrder :: Tree a -> [a]
-- levelOrder :: Tree a -> [a]
-- fromListLevelOrder :: [a] -> Tree a
