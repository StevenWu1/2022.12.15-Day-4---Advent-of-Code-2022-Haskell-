import System.IO

breakerH :: Char -> Char
breakerH x
 | x == '-' || x == ',' = ' '
 | otherwise = x

breaker :: String -> String
--breaker [] = []
--breaker (x:xs) = (breakerH x : breaker xs)
breaker xs = foldr ((:) . breakerH) [] xs
breaker2 x = words (breaker x)

mapInt = map (\x -> read x :: Int)

inclusive x
 | head x <= x!!2 && x!!1 >= x!!3 = 1
 | head x >= x!!2 && x!!1 <= x!!3 = 1
 | otherwise = 0

inclusive2 x
 | x!!2 `elem` [head x .. x!!1] = 1
 | x!!3 `elem` [head x .. x!!1] = 1
 | x!!0 `elem` [x!!2 .. x!!3] = 1
 | x!!1 `elem` [x!!2 .. x!!3] = 1
 | otherwise = 0

final xs = sum (map (inclusive . mapInt . breaker2) xs)

final2 xs = sum (map (inclusive2 . mapInt . breaker2) xs)

main = do
  x1 <- readFile "input.txt"
  let x2 = lines x1
  print(head x2)
  print(inclusive (mapInt (breaker2 "17-99,18-24")))
  print(final x2)
  print(final2 x2)
