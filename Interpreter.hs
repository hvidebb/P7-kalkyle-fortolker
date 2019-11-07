module Interpreter where
import Process

processCleanup :: Env -> Env
processCleanup = SRChain . foldr pClean [] . unChain
  where pClean :: Process -> [Process]
        pClean SRNil xs = xs
        pClean p     xs = p:xs

-- reduce :: Env -> Env
-- reduce = 
