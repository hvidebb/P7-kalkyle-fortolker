{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Process where
import Data.Function(on)

data SRProcess a = SRNil
                 | SRRecv a a (SRProcess a)
                 | SRLift a (SRProcess a) (SRProcess a)
                 | SRDrop a
                 | SRPar  { unpar :: [SRProcess a] }
                 | SRMacro String

instance Semigroup (SRProcess a) where
  (SRPar ps) <> p2 = SRPar $ ps ++ [p2]
  p1 <> (SRPar ps) = SRPar $ ps ++ [p1]
  p1 <> p2 = SRPar [p1, p2]

instance Monoid (SRProcess a) where
  mempty = SRNil

instance (a ~ String) => Show (SRProcess a) where
  show SRNil                     = "0"
  show (SRRecv c n ps@(SRPar _)) = c ++ "(" ++ n ++ ")" ++ "."
                                     ++ "(" ++ show ps ++ ")"
  show (SRRecv c n process)      = c ++ "(" ++ n ++ ")" ++ "." ++ show process
  show (SRLift c p ps@(SRPar _)) = c ++ "{" ++ show p ++ "}" ++ "."
                                     ++ "(" ++ show ps ++ ")"
  show (SRLift c p process)      = c ++ "{" ++ show p ++ "}" ++ "." ++ show process
  show (SRDrop n)                = "'" ++ n ++ "'"
  show (SRPar (p:[]))            = show p
  show (SRPar (p:ps))            = show p ++ " | " ++ show (SRPar ps)
  show (SRMacro s)               = s

type Process = SRProcess String

getChan :: (SRProcess a) -> Maybe a
getChan (SRRecv c _ _) = Just c
getChan (SRLift c _ _) = Just c
getChan _              = Nothing

modifyProcess :: (SRProcess a -> SRProcess a) -> SRProcess a -> SRProcess a
modifyProcess f = \case
  (SRRecv x y p) -> SRRecv x y (f p)
  (SRLift x y p) -> SRLift x y (f p)
  (SRPar  xs )   -> foldr1 (on (<>) $ modifyProcess f) xs
  x              -> id x
