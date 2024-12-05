
safety :: [Integer] -> (Integer -> Integer -> Integer) -> Bool
safety [] dif = True
safety [x] dif = True
safety (x : [y]) dif = n > 0 && n < 4
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

inRange :: Integer -> Bool
inRange x = n > 0 && n < 4
  where n = abs x

-- takes the input array
safety2 :: [Integer] -> Bool -> Bool
safety2 [] dropped = True
safety2 [x] dropped = True
safety2 (x : [y]) dropped  = not dropped || inRange (x - y)
safety2 (x:y:xs) False
  | rangeXY && rangeYZ && signXY == signYZ = safety2 (y:xs) False                                                        -- All good
  | otherwise                              = (safety2 (x:xs) True)  -- We don't care which one is culprit - try em all
  where z = head xs
        rangeXY = inRange (x - y)
        rangeYZ = inRange (y - z)
        signXY = signum (x - y)
        signYZ = signum (y - z)
safety2 (x:y:xs) True
  | rangeXY && rangeYZ && signXY == signYZ = safety2 (y:xs) True
  | otherwise = False
  where z = head xs
        rangeXY = inRange (x - y)
        rangeYZ = inRange (y - z)
        signXY = signum (x - y)
        signYZ = signum (y - z)

safety3 :: [Integer] -> Bool
safety3 [] = True
safety3 [x] = True
safety3 (x : [y]) = inRange (x - y)
safety3 (x:y:xs)
  | rangeXY && rangeYZ && signXY == signYZ = safety2 (y:xs) False
  | otherwise                              = (safety2 (x:xs) True) || (safety2 (y:xs) True) || (safety2 (x:y:(tail xs)) True)  -- We don't care which one is culprit - try em all
  where z = head xs
        rangeXY = inRange (x - y)
        rangeYZ = inRange (y - z)
        signXY = signum (x - y)
        signYZ = signum (y - z)

reverseList :: [Integer] -> [Integer]
reverseList = foldl (flip (:)) []

main = do
    content <- readFile "./input.txt"
    let linesOfContent :: [[Integer]] =[[read n | n <- words line] | line <- lines content]
    let res = sum (map (\x -> safetyN (safety3 x || safety3 (reverseList x))) linesOfContent)
    print res
