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
type    SymbolTable = Map.Map String Program

instance Show PEnv where
  show (PEnv (p, st)) = show p -- ++ " -- " ++ show st

-- compareProgram :: Program -> Program -> Bool
-- compareProgram [] [] = True
-- compareProgram p1 p2 = (&&) (on (==) (length) p1 p2) $
--   case bla (head p1) p2 of
--     (True, ps) -> compareProgram (tail p1) ps
--     (False, _) -> False

-- compareProcess :: PEnv -> PEnv -> Bool
-- compareProcess (PEnv (Recv c1 n1 p1, st1)) (PEnv (Recv c2 n2 p2, st2))
--   | c1 == c2 && n1 == n2 = compareProgram (insertEnv p1 st1) (insertEnv p2 st2)
--   | otherwise            = False
-- compareProcess (PEnv (Lift c1 p1, st1)) (PEnv (Lift c2 p2, st2))
--   | c1 == c2             = compareProgram (insertEnv p1 st1) (insertEnv p2 st2)
--   | otherwise            = False
-- compareProcess (PEnv (RDrop n1, st1)) (PEnv (RDrop n2, st2))
--   | n1 == n2  = True
--   | otherwise            = False

-- compareName :: (String, SymbolTable) -> (String, SymbolTable) -> Bool
-- compareName p1CST p2CST = maybe (on (==) fst p1CST p2CST) id $ do
--   p1 <- uncurry Map.lookup p1CST
--   p2 <- uncurry Map.lookup p2CST
--   return $ on compareProgram (uncurry insertEnv) (p1, snd p1CST) (p2, snd p2CST)

-- bla :: PEnv -> Program -> (Bool, Program)
-- bla penv = foldr ma (False, [])
--   where ma :: PEnv -> (Bool, Program) -> (Bool, Program)
--         ma p (b, ps)
--           | b                     = (b, p:ps)
--           | compareProcess penv p = (True, ps)
--           | otherwise             = (b, p:ps)

makeProgram :: [Process] -> Program
makeProgram = map (PEnv . (, Map.empty))

makeEnv :: Process -> PEnv
makeEnv = PEnv . (, Map.empty)

insertEnv :: [Process] -> SymbolTable -> Program
insertEnv ps st = map (PEnv . (, st)) ps

data Process = Recv  String String    [Process]
             | Lift  String           [Process]
             | RDrop String
             | Macro String
             | Ano
             deriving (Eq, Generic)

instance Hashable Process

instance Show Process where
    show (Recv c n [])  = c ++ "(" ++ n ++ ").0"
    show (Recv c n [p]) = c ++ "(" ++ n ++ ")." ++ show p
    show (Recv c n ps)  = c ++ "(" ++ n ++ ")." ++ show ps

    show (Lift c q)  = c ++ "{" ++ show q ++ "}"

    show (RDrop s)      = "'" ++ s ++ "'"
    show (Macro s)      = s
    -- show Ano            = "ሴ"
    show Ano            = "AP"
