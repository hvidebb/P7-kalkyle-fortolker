{-# LANGUAGE LambdaCase #-}
module Reduction where
import RhoProcess

import Data.List (permutations)
import Data.Bool (bool)

type Reductions = [(Process, Process)]

reduce :: Process -> [Process]
reduce = foldl reduce' [] . permutations . toList

reduce' :: [Process] -> [Operation] -> [Process]
reduce' acc = \case
  (Recv rc rn rp):(Lift lc lp):os | rc == lc ->
    ((rp \\\ (Name (getSymbol rn) (Just lp), rn)) <> (ProcessT os)):acc
  _ -> acc
