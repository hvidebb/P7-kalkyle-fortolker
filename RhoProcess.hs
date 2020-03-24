{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module RhoProcess where
import Data.List

-- {{{ Process
type Process = ProcessT Name

newtype ProcessT a = ProcessT { toList :: [OperationT a] }

instance Functor ProcessT where
  fmap f = ProcessT . map (fmap f) . toList

instance Eq a => Eq (ProcessT a) where
  (toList -> a) == (toList -> b) = elem a $ permutations b

instance Semigroup (ProcessT a) where
  (toList -> p) <> (toList -> p') = ProcessT $ p <> p'

instance Monoid (ProcessT a) where
  mempty = ProcessT $ []

instance Show a => Show (ProcessT a) where
  show p | null . toList $ p = "0"
         | otherwise = mconcat . intersperse " | " . map show . toList $ p

process :: [Operation] -> Process
process = ProcessT
-- }}}
-- {{{ Operation
type Operation = OperationT Name

data OperationT a = Recv  { getCha :: a,  getVal :: a,  getP :: ProcessT a }
                  | Lift  { getCha :: a,  getP :: ProcessT a }
                  | Drop  { getVal :: a }
                  | Scope { getP   :: ProcessT a }
  deriving (Eq)

instance Functor OperationT where
  fmap f (Recv n c p)  = Recv (f n) (f c) (fmap f p)
  fmap f (Lift n p)    = Lift (f n) (fmap f p)
  fmap f (Drop n)      = Drop (f n)
  fmap f (Scope p)     = Scope (fmap f p)

instance Show a => Show (OperationT a) where
  show (Recv n c p)  = show n ++ "(" ++ show c ++ ")." ++ case p of
    _ | (length . toList $ p) > 1 -> "(" ++ show p ++ ")"
      | otherwise    -> show p 
  show (Lift n p)    = show n ++ "{" ++ show p ++ "}"
  show (Drop n)      = show n
  show (Scope p)     = "[" ++ show p ++ "]"
-- }}}
-- {{{ Name
class IsRhoName a where
  drop  :: a -> Maybe (ProcessT a)
  quote :: String -> Maybe (ProcessT a) -> a

data Name = Name { getSymbol :: String, getQuoted :: Maybe Process }
          | NRN

instance IsRhoName Name where
  drop = getQuoted
  quote = Name 

instance Eq Name where
  (Name _ (Just a)) == (Name _ (Just b)) = a  == b
  (Name s1 Nothing) == (Name s2 Nothing) = s1 == s2
  NRN               == NRN               = True
  _                 == _                 = False

instance Show Name where
  show NRN = "Θ" 
  show x   = getSymbol x
-- }}}
-- {{{ Replaceable
replace :: Eq a => ProcessT a -> a -> a -> ProcessT a
replace p a b = fmap `flip` p $ \case
  x | x == a    ->  b
    | otherwise ->  x
-- }}}
-- {{{ Free Names
class FreeNames a where
  fn :: (IsRhoName b, Eq b) => a b -> [b]
  bn :: (IsRhoName b, Eq b) => a b -> [b]
  an :: (IsRhoName b, Eq b) => a b -> [b]

instance FreeNames ProcessT where
  fn = nub . mconcat . map fn . toList
  bn = nub . mconcat . map bn . toList 
  an = nub . mconcat . map an . toList 

instance FreeNames OperationT where
  fn (Recv c n p) = [c] ++ (delete n . nub $ fn p)
  fn (Lift c p)   = [c] ++ fn p
  fn (Drop n)     = [n]
  fn (Scope p)    = fn p

  bn (Recv c n p) = [n] ++ bn p
  bn (Lift c p)   = bn p
  bn (Drop n)     = []
  bn (Scope p)    = bn p

  an (Recv c n p) = [c, n] ++ an p
  an (Lift c p)   = [c] ++ an p
  an (Drop n)     = [n]
  an (Scope p)    = bn p
-- }}}
