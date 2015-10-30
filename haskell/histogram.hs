module Histogram where
import Data.List
import Data.Maybe

type Histogram a = [(a, Int)]

histogram :: (Ord a, Eq a) => [a] -> [(a, Int)]
histogram xs = 
    let groups = group (sort xs)
     in zip (map head groups) (map length groups)

occurances :: (Eq a) => a -> Histogram a -> Int
occurances x hst =  fromMaybe 0 $ lookup x hst

values :: Histogram a -> [a]
values hst = map fst hst

counts :: Histogram a -> [Int]
counts hst = map snd hst
