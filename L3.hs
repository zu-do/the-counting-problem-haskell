import System.IO ()

main :: IO ()
main = do
    contents <- readFile "data.txt"
    print contents
    let list = lines contents
    writeFile "results.txt" $ unlines $ map (show . mainFunction . readInt ) list

mainFunction :: (Int, Int, [Int]) -> [Int]
mainFunction (x, y, z) 
  | x < 1 && y < 1 = [0]
  | otherwise = map (countRecursion (concat (map digits (rangeList x y)))) z

--parse input in a correct form
readInt :: String -> (Int, Int, [Int])
readInt line = do
  let wordList = words line
  (read (head wordList) :: Int, read (wordList !!1) :: Int, [0,1..9] ::[Int])

--gets all numbers between to ints
rangeList :: Int -> Int -> [Int]
rangeList n m
  | n < m = [n, n+1..m]
  | n > m = [m, m+1..n]
  | otherwise = [0]

--where a is and integral type support  int division
--from numbers list to digits
digits :: (Integral a) => a -> [a]
digits 0 = [0]
digits n = go (abs n) [] where
  go 0 digs = digs
  go x digs = go (x `div` 10) (x `mod` 10 : digs)

--counts occuriences of int value in a list
countRecursion :: Eq a => [a] -> a -> Int
countRecursion [] find = 0
countRecursion (x:xs) find 
    | find == x = 1 + (countRecursion xs find)
    | otherwise = countRecursion xs find