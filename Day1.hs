-- Part 1
increasing :: [Integer] -> Integer
increasing (x:xs) = increasing' 0 x xs
increasing [] = 0

increasing' :: Integer -> Integer -> [Integer] -> Integer
increasing' count prev [] = count

increasing' count prev (x:xs) = if (x > prev)
  then increasing' (count + 1) x xs
  else increasing' (count) x xs

-- Part 2
increasing3 :: [Integer] -> Integer
increasing3 [] = 0
increasing3 (x0:[]) = 0
increasing3 (x0:x1:[]) = 0
increasing3 (x0:x1:x2:[]) = 0
increasing3 (x0:x1:x2:xs) = increasing3' 0 (x0 + x1 + x2) (x1:x2:xs)

increasing3' :: Integer -> Integer -> [Integer] -> Integer
increasing3' count prev [] = count
increasing3' count prev (x0:[]) = count
increasing3' count prev (x0:x1:[]) = count
increasing3' count prev (x0:x1:x2:xs) = if ((x0 + x1 + x2) > prev)
  then increasing3' (count + 1) (x0 + x1 + x2) (x1:x2:xs)
  else increasing3' (count) (x0 + x1 + x2) (x1:x2:xs)

-- Output
main = do
  let xxx = [] -- https://adventofcode.com/2021/day/1/input
  print (show (increasing xxx))
  print (show (increasing3 xxx))
