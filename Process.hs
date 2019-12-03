{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Process where
import Data.Function(on)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Hashable
import GHC.Generics (Generic)

newtype PEnv        = PEnv { unPEnv :: (Process, SymbolTable) }
                      deriving (Eq)
type    Program     = [PEnv]
type    SymbolTable = Map.Map String [Process]

instance Show PEnv where
  show (PEnv (p, st)) = show p -- ++ " -- " ++ show st

makeEnv :: [Process] -> Program
makeEnv = map (PEnv . (, Map.empty))

insertEnv :: [Process] -> SymbolTable -> Program
insertEnv ps st = map (PEnv . (, st)) ps

-- isProgramEq :: Program -> Program -> Bool
-- isProgramEq ps1 ps2 = foldr foldh ps1 True
--   where foldh p1 acc = (foldl (\p2 acc' -> p1 == p2 || acc) False ps2) && acc

data Process = Recv  String String    [Process]
             | Lift  String [Process]
             | RDrop String
             | Macro String
             | Ano deriving (Eq, Generic)

instance Hashable Process

instance Show Process where
    show (Recv c n [])  = c ++ "(" ++ n ++ ").0"
    show (Recv c n [p]) = c ++ "(" ++ n ++ ")." ++ show p
    show (Recv c n ps)  = c ++ "(" ++ n ++ ")." ++ show ps

    show (Lift c q)  = c ++ "{" ++ show q ++ "}"

    show (RDrop s)      = "'" ++ s ++ "'"
    show (Macro s)      = s
    -- show Ano            = "ሴ"
    show Ano            = "ANON"
