toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n <= 0 = []
  | div n 10 == 0 = n:[]
  | otherwise = (mod n 10):(toDigitsRev (div n 10))


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs
  =  [if (mod x 2) == 0 then y else y*2 |
      x <- [0..((length xs)-1)] , let y = xs !! x]


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) =
  let y = div x 10
      z = mod x 10
  in y + z + (sumDigits xs)


validate :: Integer -> Bool
validate x =
  sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi 2 a b c = [(a,c), (a,b), (c,b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a,b)]
hanoi4 2 a b c _ = [(a,c), (a,b), (c,b)]
hanoi4 n a b c d = [(a,c), (a,d), ]

