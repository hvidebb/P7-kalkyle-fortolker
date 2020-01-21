module Main where
import Interpreter
import Parser
import Process
import Kripke
import Control.Monad.State
import Data.IntMap.Internal.Debug
import Data.Hashable
import qualified Data.IntMap as IM

-- TODO:
--  Symboltabel til lift ved reduering
--  Tjek ved symbol ved reducering

main :: IO ()
main = print "Hallo"

testproc :: Program
testproc = makeProgram . testParse $ "a(b).'b' | c(d).'d'"

test :: IO ()
test = do
  print testproc
  putStrLn $ replicate 80 '='
  kripke <- return . buildKripke $ testproc
  IM.foldMapWithKey (curry print) . fst $ kripke
  putStrLn $ replicate 80 '-'
  IM.foldMapWithKey (curry print) . snd $ kripke
  return $ seq kripke $ ()

printPrograms :: Maybe [Program] -> IO ()
printPrograms (Just ps) = do
  foldMap printProgram ps
printPrograms x         = print x

printProgram :: Program -> IO ()
printProgram p = do
  case p of
    [] -> putStrLn "0"
    p  -> foldMap print p
  putStrLn "--------------------"
  
