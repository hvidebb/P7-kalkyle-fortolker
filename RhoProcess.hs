{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module RhoProcess where
import Data.List
import Data.Bool

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

instance Eq a => Eq (OperationT a) where
  (Recv  c n p) == (Recv  c' n' p') = c == c' && (rn n p) == (rn n' p')
  (Lift  c p)   == (Lift  c' p')    = c == c' && p == p'
  (Drop  n)     == (Drop  n')       = n == n'
  (Scope p)     == (Scope p')       = p == p'

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

rn :: Eq a => a -> ProcessT a -> ProcessT (Maybe a)
rn n = fmap (\n' -> bool (Just n') Nothing (n == n'))

-- }}}
-- {{{ Name
data Name = Name { getSymbol :: String, getQuoted :: Maybe Process }

instance Eq Name where
  (Name _ (Just a)) == (Name _ (Just b)) = a  == b
  (Name s1 Nothing) == (Name s2 Nothing) = s1 == s2
  _                 == _                 = False

instance Show Name where
  show x   = getSymbol x
-- }}}
-- {{{ Replaceable
infixr 9 \\\
(\\\) :: Process -> (Name, Name) -> Process
p \\\ (a, b) = replace a b p

replace :: Name -> Name -> Process -> Process
replace ner ned = ProcessT . foldl replace' [] . toList
  where replace' :: [Operation] -> Operation -> [Operation]
        replace' acc = \case 
          (Recv  c n p) -> 
            (Recv (replaceN c) (replaceN n) (replace ner ned p)):acc
          (Lift  c p)   -> (Lift (replaceN c) (replace ner ned p)):acc
          (Drop  n)     | n == ned ->
            case getQuoted ner of
              Just p  -> acc ++ toList p
              Nothing -> (Drop ner):acc
                        | otherwise -> (Drop n):acc
          (Scope p) -> (Scope $ replace ner ned p):acc
        replaceN name | ned == name = ner
                      | otherwise   = name
-- }}}
