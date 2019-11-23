import           Data.Char

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

-- lesson 8
myLength :: [Int] -> Int
myLength []       = 0
myLength (_ : xs) = 1 + myLength xs

myReverse :: [Char] -> [Char]
myReverse [] = []
myReverse x  = (last x) : (myReverse (init x))

fastFib :: Int -> Int -> Int -> Int
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

-- lesson 9
remove :: (Int -> Bool) -> [Int] -> [Int]
remove test []       = []
remove test (x : xs) = if test x then remove test xs else x : remove test xs

myProduct :: [Int] -> Int
myProduct []    = 1
myProduct aList = foldl (*) 1 aList

myElem :: Int -> [Int] -> Bool
myElem n aList = (length filtered) > 0
    where filtered = filter (\x -> x == n) aList

myIsNotSpace :: Char -> Bool
myIsNotSpace c = c /= ' '

myToLowerCase :: Char -> Char
myToLowerCase c = toLower c


isPalindrome :: String -> Bool
isPalindrome str = processedStr == reverse processedStr
    where processedStr = map myToLowerCase (filter myIsNotSpace str)

harmonic :: Int -> Double
harmonic 0 = 0.0
harmonic n = foldl (+) 0.0 (map (\x -> 1.0 / x) (take n [1.0, 2.0 ..]))

-- lesson 14
data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Enum, Eq, Show)
class (Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5)
