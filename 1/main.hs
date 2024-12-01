popMin :: [Integer] -> (Integer, [Integer])
popMin [] = (0, [])
popMin [x] = (x, [])
popMin (x:[y]) = (min x y, [max x y])
popMin (x:xs)
    | x > xsMin = (xsMin, x     : xsRem)
    | otherwise = (x,     xsMin : xsRem)
    where (xsMin, xsRem) = popMin xs


sumSmallest :: [Integer] -> [Integer] -> Integer
sumSmallest [] [] = 0
sumSmallest [x] [y] = abs (x - y)
sumSmallest a b = abs (aMin - bMin) + sumSmallest aRem bRem
    where (aMin, aRem) = popMin a
          (bMin, bRem) = popMin b

splitSpace :: String  -> (String, String)
splitSpace [] = ("", "")
splitSpace " " = ("", "")
splitSpace (c:cs)
    | c == ' ' = ("", cs)
    | otherwise = (c : p1, p2)
    where (p1, p2) = splitSpace cs
    

splitLines :: [String] -> ([Integer], [Integer])
splitLines [] = ([], [])
splitLines [x] = ([read one], [read two])
    where (one, two) = splitSpace x
splitLines (x:xs) = (read one : restA, read two : restB)
    where (one, two) = splitSpace x
          (restA, restB) = splitLines xs
    

main = do
    content <- readFile "./input.txt"
    let linesOfContent :: [String] = lines content
    let (listA, listB) = splitLines linesOfContent
    let res = sumSmallest listA listB
    print res
