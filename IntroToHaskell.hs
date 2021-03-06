import Data.List

delete_all_a :: (Eq a) => a -> ([a] -> [a])
delete_all_a _ [] = []
delete_all_a b (x:xs)
  | b == x         = delete_all_a b xs 
  | otherwise      = x : delete_all_a b xs


delete_all_b :: (Eq a) => a -> ([a] -> [a])
delete_all_b a xs = [x | x <- xs, x /= a]


delete_all_c :: (Eq a) => a -> ([a] -> [a])
delete_all_c a xs = filter (/=a) xs
 

del_fstnscnd_a :: (Eq a) => a -> ([a] -> [a])
del_fstnscnd_a _ [] = []
del_fstnscnd_a a (x:xs) 
  | a == x     =   del_fstnscnd_helper a xs
  | otherwise = x : del_fstnscnd_a a xs 


del_fstnscnd_helper :: (Eq a) => a -> ([a] -> [a])
del_fstnscnd_helper _ [] = []
del_fstnscnd_helper a (x:xs) 
  | a == x     = xs
  | otherwise = x : del_fstnscnd_helper a xs 


del_fstnscnd_b :: (Eq a) => a -> ([a] -> [a])
del_fstnscnd_b a list = (delete a (delete a list))


merge :: (Ord a) => [[a]] -> [a]
merge list = (sort_merge (concat list))


mergeU :: (Ord a) => [[a]] -> [a]
mergeU list = nub (merge list)


sort_merge :: (Ord a, Eq a) => [a] -> [a]
sort_merge list = sort (list)


sum_votes_if :: String -> [(String, Int)] -> (Int, Bool)
sum_votes_if s list 
  | x == 0      = (0, False)
  | otherwise   = (x, True)
  where x = sum_votes (filter ((==s).fst) list)


sum_votes :: [(String, Int)] -> Int
sum_votes [] = 0
sum_votes ((s, x):xs) = x + sum_votes xs 


sumHarmonic :: (Fractional a, Eq a, Enum a) => a -> a
sumHarmonic m = sum[1/x | x <- [1..m]]


evaluateFunc :: (Fractional a, Eq a, Enum a) => a -> Bool
evaluateFunc m
  | sum[x^3 | x<- [0 ..m]] == x    = True
  | otherwise                      = False
  where x = (((m * (m + 1)) / 2) *  ((m * (m + 1)) / 2))


evaluateFunc_a ::(Fractional a, Eq a, Enum a) => a -> a
evaluateFunc_a m
  | sum[x^3 | x<- [0 ..m]] == x    = x
  | otherwise                      = 0
  where x = (((m * (m + 1)) / 2) *  ((m * (m + 1)) / 2))


string2word :: String -> [String]
string2word x = words (x)


indexOf :: (Eq t) => t -> [t] -> Int
indexOf _ [] = -1
indexOf a (x:xs)
  | a == x    = 0
  | n < 0     = n  
  | otherwise = n + 1
  where n = indexOf a xs 


approximations :: (Real a, Fractional a) => a -> a -> [a]
approximations n a0 = iterate (\a0 -> ((1/2)*(a0 + n/a0)))  a0



composeFunc :: [(a -> a)] -> (a -> a)
composeFunc [] list = list
composeFunc (x:xs) list  = (x (composeFunc xs list))





main = do

  putStrLn "  delete_all_a Test"
  print (delete_all_a 1 [1, 2, 3, 2, 1, 2, 3, 2, 1])
  print (delete_all_a 4 [1, 2, 3, 2, 1, 2, 3, 2, 1])
  print (delete_all_a 3 [1, 2, 3])
  putStrLn " "
  putStrLn "  delete_all_b Test"
  print (delete_all_b 1 [1, 2, 3, 2, 1, 2, 3, 2, 1])
  print (delete_all_b 4 [1, 2, 3, 2, 1, 2, 3, 2, 1])
  print (delete_all_b 3 [1, 2, 3])
  putStrLn " "
  putStrLn "  delete_all_c Test"
  print (delete_all_c 1 [1, 2, 3, 2, 1, 2, 3, 2, 1] )
  print (delete_all_c 4 [1, 2, 3, 2, 1, 2, 3, 2, 1])
  print (delete_all_c 3 [1, 2, 3])
  putStrLn " "
  putStrLn "  del_fstnscnd_a Test"
  print (del_fstnscnd_a 1 [1, 2, 3, 2, 1, 2, 3, 2, 1])
  print (del_fstnscnd_a 4 [1, 2, 3, 2, 1, 2, 3, 2, 1, 4 ,4])
  print (del_fstnscnd_a 3 [1, 2, 3])
  putStrLn " "
  putStrLn "  del_fstnscnd_b Test"
  print (del_fstnscnd_b 1 [1, 2, 3, 2, 1, 2, 3, 2, 1])
  print (del_fstnscnd_b 4 [1, 2, 3, 2, 1, 2, 3, 2, 1, 4 ,4])
  print (del_fstnscnd_b 3 [1, 2, 3])
  putStrLn " "
  putStrLn "  merge Test"
  print (merge [[1, 2, 3]])
  print (merge [[1, 3, 5, 7], [2, 4, 6]])
  print (merge [[1,3,5,7], [2,4,6], [3,5,9,10,11,12]])
  print (take 8 (merge [[1, 3, 5, 7], [1,2,3,4,5,6,7,8]]))
  putStrLn " "
  putStrLn "  mergeU Test"
  print (mergeU [[1, 2, 3]])
  print (mergeU [[1, 3, 5, 7], [2, 4, 6]])
  print (mergeU [[1,3,5,7], [2,4,6], [3,5,9,10,11,12]])
  print (take 8 (mergeU [[1, 3, 5, 7], [1,2,3,4,5,6,7,8]]))
  putStrLn " "
  putStrLn "  sum_votes_if Test"
  print (sum_votes_if "John" [])
  print (sum_votes_if "John" [("Adam", 6), ("Bob",3)])
  print (sum_votes_if "John" [("John", 4), ("John", 7), ("Adam", 6), ("Bob",3)])
  putStrLn " "
  putStrLn "  SumHarmonic Test"
  putStrLn "sumHarmonic 4"
  print (sumHarmonic 4)
  putStrLn "sumHarmonic 5"
  print (sumHarmonic 5)
  putStrLn "sumHarmonic 6"
  print (sumHarmonic 6)
  putStrLn " "
  putStrLn "  EvaluateFunc Test"  
  putStrLn "  EvaluateFunc for values 1-20"
  print (evaluateFunc_a 1)
  print (evaluateFunc_a 2)
  print (evaluateFunc_a 3)
  print (evaluateFunc_a 4)
  print (evaluateFunc_a 5)
  print (evaluateFunc_a 6)
  print (evaluateFunc_a 7)
  print (evaluateFunc_a 8)
  print (evaluateFunc_a 9)
  print (evaluateFunc_a 10)
  print (evaluateFunc_a 11)
  print (evaluateFunc_a 12)
  print (evaluateFunc_a 13)
  print (evaluateFunc_a 14)
  print (evaluateFunc_a 15)
  print (evaluateFunc_a 16)
  print (evaluateFunc_a 17)
  print (evaluateFunc_a 18)
  print (evaluateFunc_a 19)
  print (evaluateFunc_a 20)

  putStrLn " "
  putStrLn "  string2word Test"
  print (string2word "This is a test")
  print (string2word "This is a        test")
  print (string2word "My number is 1234")
  putStrLn " "
  putStrLn "  indexOf Test"
  print (indexOf 2 [1,3,5,4,3,5])
  print (indexOf 2 [1,3,5,2,4,3,5])
  print (indexOf "abc" ["bb", "abc", "cccc", "zxy"])
  putStrLn " "
  putStrLn "  approximations Test"
  print (take 6(approximations 1.0 1.0))
  print (take 5 (approximations 2.0 1.0))
  print (take 5 (approximations 64.0 1.0))
  putStrLn " "
  putStrLn "  composeFunc Test"
  print (composeFunc [] [1, 2, 3])
  print (composeFunc[tail, init] [1, 2, 3, 4, 5])
  
  



  

  
