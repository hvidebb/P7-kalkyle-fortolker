module Bisimular where

class Bisimular a where
  (~~) :: a -> a -> Bool
  (/~) :: a -> a -> Bool
