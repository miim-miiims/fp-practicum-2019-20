module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where


group ::Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (x:xs) = ys:group zs
            where (ys, zs) = span  (== x) (x:xs)  

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ x [] = [x]
insertBy f x (y:ys) = if f x y == LT then x:y:ys else y:insertBy f x ys

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:xs) = insertBy f x (sortBy f xs)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy _ [x] = [[x]]
groupBy f (x:xs) = ys:groupBy f zs
                   where (ys,zs) = span (f x) (x:xs)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x, g x)


insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn _ x [] = [x]
insertOn f x (y:ys) = if (compare `on` f) x y == GT then y:insertOn f x ys else x:y:ys

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f (x:xs) = insertOn f x (sortOn f xs)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn _ [] = []
groupOn _ [x] = [[x]]
groupOn f (x:xs) = ys:groupOn f zs
                   where (ys,zs) = span (\y -> f y == f x) (x:xs)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f xs = groupOn f $ sortOn f xs
