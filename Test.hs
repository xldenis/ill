module Test where

class Add a  where
  add :: a -> a -> a
  zero :: a

instance Add Int where
  add = (+)
  zero = 0

omg :: Int
omg = 1 `add` 2

omg2 a b = a `add` b
