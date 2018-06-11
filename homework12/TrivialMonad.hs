{-# GHC_OPTIONS -Wall #-}

module TrivialMonad where

data W a = W a deriving Show

return' :: a -> W a
return' = W

fmap' :: (a -> b) -> (W a -> W b)
fmap' f (W x) = W (f x)

bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

g :: Int -> W Int -> W Int
g x w = bind f w
  where f y = return' (x+y)

h :: W Int -> W Int -> W Int
h w = fmap' (+) (bind g w)
  where g  = return'
