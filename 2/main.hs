
safety :: [Integer] -> (Integer -> Integer -> Integer) -> Bool
safety [] dif = True
safety [x] dif = True
safety (x : [y]) dif = n > 1 && n < 3
  where n = dif x y
safety (x:y:xs) dif = safety [x, y] dif && safety (y:xs) dif

safeWrap :: [Integer] -> Integer
safeWrap [] = 1
safeWrap [x] = 1
safeWrap (x:y:xs) 
  | x > y     = safetyN (safety (x:y:xs) (-))
  | otherwise = safetyN (safety (x:y:xs) (flip (-)))

safetyN :: Bool -> Integer
safetyN x
  | x = 1
  | otherwise = 0


main = do
    content <- readFile "./input.txt"
    let linesOfContent :: [[Integer]] =[[read n | n <- words line] | line <- lines content]
    let res = sum (map safeWrap linesOfContent)
    print res
