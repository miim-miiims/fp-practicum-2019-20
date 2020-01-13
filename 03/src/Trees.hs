{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  Node x l r == Node y ll rr = l == ll && x == y && r == rr  
  _ == _ = False

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty = Node x Empty Empty
insertOrdered x (Node y l r) = if x <= y then Node y (insertOrdered x l) r else Node y l (insertOrdered x r) 

listToBST :: Ord a => [a] -> Tree a
listToBST = foldr insertOrdered Empty 

isBST :: Ord a => Tree a -> Bool
isBST = between Bot Top 

-- idea for implementing isBST - delete if you don't want it
data BotTop a = Bot | Val a | Top
  deriving (Show, Eq, Ord)

between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
between _ _ Empty = True
between x Top (Node y l _) = between x (Val y) l
between Bot x (Node y _ r) = between (Val y) x r  
between (Val x) (Val y) (Node z l r) = (x <= z && z <= y) && (between (Val x) (Val z) l && between (Val z) (Val y) r) 
between _ _ _ = False

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST x (Node y l r) = (x == y) || (findBST x l || findBST x r) 

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r) 

foldTree :: Monoid a => Tree a -> a
foldTree = undefined

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree = undefined

sumTree :: Num a => Tree a -> a
sumTree = undefined

allTree :: (a -> Bool) -> Tree a -> Bool
allTree = undefined

treeToList :: Tree a -> [a]
treeToList = undefined

elemTree :: Eq a => a -> Tree a -> Bool
elemTree = undefined

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred = undefined

findAll :: (a -> Bool) -> Tree a -> [a]
findAll = undefined

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree = undefined

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch = undefined

paths :: Tree a -> [(a, [Direction])]
paths = undefined
