{-# LANGUAGE TypeFamilies #-}
module Process where

data SREnv a = SRChain { unChain :: [SRProcess a] }

data SRProcess a = SRNil
                 | SRRecv a a (SRProcess a)
                 | SRLift a a (SRProcess a)
                 | SRDrop a
                 | SRPar  { unpar :: [SRProcess a] }
                 | SRMacro String

instance (a ~ String) => Show (SRProcess a) where
  show SRNil                     = "0"
  show (SRRecv c n ps@(SRPar _)) = c ++ "(" ++ n ++ ")" ++ "."
                                     ++ "(" ++ show ps ++ ")"
  show (SRRecv c n process)      = c ++ "(" ++ n ++ ")" ++ "." ++ show process
  show (SRLift c n ps@(SRPar _)) = c ++ "(" ++ n ++ ")" ++ "."
                                     ++ "(" ++ show ps ++ ")"
  show (SRLift c n process)      = c ++ "{" ++ n ++ "}" ++ "." ++ show process
  show (SRDrop n)                = "'" ++ n ++ "'"
  show (SRPar (p:[]))            = show p
  show (SRPar (p:ps))            = show p ++ "|" ++ show (SRPar ps)
  show (SRMacro s)               = s

type Process = SRProcess String

type Env = SREnv String

instance (a ~ String) => Show (SREnv a) where
  show = show . unChain
