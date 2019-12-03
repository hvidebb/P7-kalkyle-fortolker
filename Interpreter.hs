{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Interpreter where
import Control.Monad.Writer
import Process
import Data.Bool
import Data.Function
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

import qualified Data.Map.Strict as Map

stepProcess :: Program -> Maybe [Program]
stepProcess = fmap (map dropSubs) . (reduce comVoid <> reduce comTau)
-- stepProcess = map dropSubs . reduce comTau

type PReducer = Process -> SymbolTable -> Program -> Maybe [Program]

-- cleanAnon :: Program -> Program
-- cleanAnon = foldr foldh ([], False)
--   where 

dropSubs :: Program -> Program
dropSubs = foldr foldf []
  where foldf :: PEnv -> Program -> Program
        foldf p@(PEnv (RDrop n, st)) acc = case Map.lookup n st of
          Just np -> acc ++ insertEnv np st
          Nothing -> p:acc
        foldf a acc = a:acc

reduce :: PReducer -> Program -> Maybe [Program]
reduce f = foldr (uncurry fhelp) Nothing . \lst -> zip (inits lst) (tails lst)
  where fhelp lst [] acc     = acc
        fhelp lst (x:xs) acc = map (++ lst) <$>
                               (uncurry f) (unPEnv x) xs <> acc

comVoid :: PReducer
comVoid (Lift c q)   st xs = Just [xs]
comVoid (Recv c n p) st xs = Just $ [insertEnv p nst ++ xs]
  where nst = Map.insert n [Ano] st
comVoid _ _ _ = Nothing

comTau :: PReducer
comTau pL stL = reduce comproc
  where comproc :: PReducer
        comproc pR stR xs = do
          nxs <- tauReduce (PEnv (pL, stL)) (PEnv (pR, stR))
          return $ [nxs ++ xs]

tauReduce :: PEnv -> PEnv -> Maybe Program
tauReduce envL envR = do
  (liftC, liftP, liftST)        <- on (<|>) getLift envL envR
  (recvC, recvN, recvP, recvST) <- on (<|>) getRecv envL envR
  compareProcess (liftC, liftST) (recvC, recvST)
    <|> (guard $ liftC == recvC)
  Just $ insertEnv recvP (Map.insert recvN liftP recvST)
  where compareProcess liftCST recvCST = do
          liftPname <- uncurry Map.lookup $ liftCST
          recvPname <- uncurry Map.lookup $ recvCST
          guard $ liftPname == recvPname

getLift :: PEnv -> Maybe (String, [Process], SymbolTable)
getLift (PEnv (Lift c p, st)) = Just $ (c, p, st)
getLift _                     = Nothing

getRecv :: PEnv -> Maybe (String, String, [Process], SymbolTable)
getRecv (PEnv (Recv c n p, st)) = Just $ (c, n, p, st)
getRecv _                       = Nothing

substiRecv :: [Process] -> PEnv -> Maybe Program
substiRecv ps (PEnv (Recv c n np, st)) = Just $ insertEnv np nst
  where nst = Map.insert n ps st
substiRecv _ _ = Nothing
