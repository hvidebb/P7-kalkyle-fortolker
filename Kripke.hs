{-# LANGUAGE Strict #-}
module Kripke where
import Process
import Interpreter
import Control.Monad.State
import Control.Monad
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.IntMap.Internal.Debug
import Data.Tuple.Extra (first, second)

type Kripke a = State KGraph a

type KGraph   = (KPMap, KEMap)
type KPMap    = IM.IntMap Program
type KEMap    = IM.IntMap [IM.Key]

buildKripke :: Program -> KGraph
buildKripke p = execState state (IM.empty, IM.empty)
  where state = (:[]) <$> makeKripke p >>= completeKripke

makeKripke :: Program -> Kripke Int
makeKripke p = do
  kpmap <- gets fst
  id    <- return . fromMaybe 0 $ (+1) . fst <$> IM.lookupMax kpmap
  modify . first $ updateKPMap id p
  return id

completeKripke :: [Int] -> Kripke ()
completeKripke [] = return ()
completeKripke (i:is) = do
  cp  <- gets $ IM.lookup i . fst
  ids <- mapM makeKripke . concat $ cp >>= stepProcess
  modify . second $ updateKEMap i ids
  completeKripke $ is ++ ids

updateKPMap :: Int -> Program -> KPMap -> KPMap
updateKPMap = IM.insert 

updateKEMap :: Int -> [Int] -> KEMap -> KEMap
updateKEMap = IM.insert

ktrace :: Kripke ()
ktrace = do
  (kpmap, kemap) <- get
  trace ("Debug: " ++ show kpmap ++ "\n" ++ show kemap) $ return ()
