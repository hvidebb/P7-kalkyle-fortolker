{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Interpreter where
import Process
import Data.Bool
import Data.Function
import Data.List
import Data.Maybe

data SREnv a = SREnv { unSREnv :: [SRProcess a] }

type Env = SREnv String

instance (a ~ String) => Show (SREnv a) where
  show = foldMap (++ "\n") . map show . unSREnv

makeEnv :: SRProcess a -> SREnv a
makeEnv (SRPar xs) = SREnv xs
makeEnv p = SREnv $ [p]

stepProcess :: Ord a => SREnv a -> (SREnv a, SREnv a)
stepProcess (SREnv xs) = (SREnv ncoms, SREnv coms)
  where (ncoms, coms) = foldr (\v (al, ar) -> bool (al, v : ar) (v : al, ar) . isNothing . getChan $ v) ([], []) xs


