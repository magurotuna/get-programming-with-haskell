main :: IO ()
main = print "Hello World"

minus2 :: Int -> Int
minus2 x = x - 2

subtract2 :: Int -> Int
subtract2 = flip (-) 2

getRequestUrl :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestUrl host apiKey resource id =
    host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

exampleUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char]
exampleUrlBuilder = getRequestUrl "https://example.com"

myExampleUrlBuilder :: [Char] -> [Char] -> [Char]
myExampleUrlBuilder = exampleUrlBuilder "1337hAsk311"

myRepeat :: Int -> [Int]
myRepeat n = cycle [n]

subseq :: Int -> Int -> [Char] -> [Char]
subseq start end str = drop start (take end str)

inFirstHalf :: Int -> [Int] -> Bool
inFirstHalf x list = x `elem` firstHalf
    where firstHalf = take (length list `div` 2) list

myTail :: [Int] -> [Int]
myTail []       = []
myTail (_ : xs) = xs

myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)
