import Data.List

type Set a = [a]
empty :: Set Int
empty = []

memberofSet_ :: (Eq a) => Set a -> a -> Bool
memberofSet_ set element = element `elem` set

union_ :: (Ord a) => Set a -> Set a -> Set a
union_ set1 set2 = sort (removeduplicates(set1 ++ set2))

-- Removes Duplicates from 
removeduplicates :: Eq a => [a] -> [a]
removeduplicates [] = []
removeduplicates (x:xs)   
    | x `elem` xs   = removeduplicates xs
    | otherwise     = x : removeduplicates xs

intersect_ :: Ord a => Set a -> Set a -> Set a 
intersect_ a b = removeduplicates ([x | x <- a, x `elem` b])

difference_ :: Ord a => Set a -> Set a -> Set a
difference_ a b = removeduplicates([x | x <-a, not(x `elem` b)]) 

equalSet_ :: (Eq a, Ord a) => Set a -> Set a -> Bool 
equalSet_ a b
  | (length (a) == length (b)) && (length(intersect_ a b) == length (a)) && (length (intersect_ a b) == length (b))     = True
  | otherwise     = False      

subSet_ :: (Ord a) => Set a -> Set a -> Bool
subSet_ (x:xs) (y:ys) =  subSetHelp xs ys

subSetHelp :: Ord a => [a] -> [a] -> Bool
subSetHelp [] ys = True
subSetHelp xs [] = False
subSetHelp (x:xs) (y:ys) 
    | y > x              = False
    | x == y             = subSetHelp xs ys
    | x > y              = subSetHelp (x:xs) ys


mapSet_ :: Ord b => (a -> b) -> Set a -> Set b
mapSet_ map_ unMappedSet = map map_ unMappedSet

filterSet_ :: (a -> Bool) -> Set a -> Set a 
filterSet_ (filter_) unfilteredSet = filter filter_ unfilteredSet


foldSet_ :: (a -> a -> a) -> a -> Set a -> a 
foldSet_ fold f s = foldr fold f s

showSet :: Show a => Set a -> String
showSet a = show a



main = do

  putStrLn ("Testing empty")
  putStrLn ("EmptySet empty = " ++ showSet (empty))
  putStrLn " "
  putStrLn ("Testing memberofSet_")
  putStrLn ("memberofSet_ [1,2,3] 1 = " ++ show( memberofSet_ [1,2,3] 1))

  putStrLn ("memberofSet_ [\"A\",\"B\",\"C\"] \"A\" = " ++ show (memberofSet_ ["A","B","C"] "A"))

  putStrLn " "
  putStrLn ("Testing union")
  putStrLn ("union [4, 3, 6, 1] [1,5,7] = "  ++ showSet (union_ [4, 3, 6, 1] [1,5,7]))
  

  putStrLn " "
  putStrLn ("Testing intersect_")
  putStrLn ("intersect_ [4, 3, 6, 1] [1,5,7] = "  ++ showSet (intersect_ [4, 3, 6, 1, 1] [1,1,5,7]))

  putStrLn " "
  putStrLn ("Testing difference_")
  putStrLn ("difference_ [1, 2, 3, 4, 5] [3, 4, 5, 6, 7, 8] = " ++ showSet (difference_ [1, 2, 3, 4, 5] [3, 4, 5, 6, 7, 8]))

  putStrLn " "
  putStrLn ("Testing equalSet_")
  putStrLn ("equalSet_ [1,2,3] [1,2,3] = " ++ show (equalSet_ [1,2,3][1,2,3]))

  putStrLn " "
  putStrLn ("Testing mapSet_")
  putStrLn ("mapSet_ (+2) [2,4,6,8] = " ++ showSet (mapSet_ (+2) [2,4,6,8]))
  putStrLn " "
  putStrLn ("Testing subSet_")
  putStrLn ("subSet_ [1,2,3] [1,2,3,4,5] = " ++ show (subSet_ [1,2,3] [1,2,3,4,5]))

  putStrLn " "
  putStrLn ("Testing filterSet_")
  putStrLn ("filterSet_ (==2) [2,4,6,2,8,4,2] = " ++ showSet (filterSet_ (==2) [2,4,6,2,8,4,2]))

  putStrLn " "
  putStrLn ("Testing foldSet_")
  putStrLn ("foldSet_  (+) 0 [1,2,3,4] = " ++ show (foldSet_  (+) 0 [1,2,3,4]))

  putStrLn " "
  putStrLn ("Testing showSet")
  putStrLn ("showSet [1,2,3,4,5,6] = " ++ showSet ([1,2,3,4,5,6]))
