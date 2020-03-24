module ParserCompinators where
import Data.Functor
import Data.Function
import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    p input >>= \(input', x) -> return $ (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    return (input'', f a)

instance Monad Parser where
  return = pure
  (Parser p1) >>= f = Parser $ \input -> do
    (input', a) <- p1 input
    (Parser p2) <- return $ f a
    p2 input'

instance Alternative Parser where
  empty = Parser . const $ empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Semigroup a => Semigroup (Parser a) where
  (Parser p1) <> (Parser p2) = Parser $ \input -> do
    (input', x1) <- p1 input
    (input'', x2) <- p2 input'
    return $ (input'', x1 <> x2)

instance Monoid a => Monoid (Parser a) where
  mempty = Parser . const $ mempty

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \input ->
  case input of
    y:ys | f y -> Just (ys, y)
    _          -> Nothing

symbol :: String -> Parser String
symbol s = stringP s <* manyP isSpace

choice :: [Parser a] -> Parser a
choice = foldr1 (<|>) 

count :: Int -> Parser a -> Parser [a]
count n = sequenceA . take n . repeat 

between :: Parser open -> Parser close -> Parser a -> Parser a
between p1 p2 pM = p1 *> pM <* p2

option :: a -> Parser a -> Parser a
option a (Parser p) = Parser $ \input ->
  case p input of
    Just x -> Just x
    Nothing -> Just (input, a)

sepSome :: Parser a -> Parser sep -> Parser [a]
sepSome pA pSep = do
  lst <- (some (pA <* pSep)) <> (fmap (:[]) pA)
  return lst

sepMany :: Parser a -> Parser sep -> Parser [a]
sepMany pA pSep = do
  lst <- (many (pA <* pSep)) <> (fmap (:[]) pA)
  return lst

failed :: Parser a -> Parser Bool
failed (Parser pA) = Parser $ \input ->
  case pA input of
    Just _  -> Just (input, False)
    Nothing -> Just (input, True)


spaces :: Parser ()
spaces = void . many . satisfy $ isSpace

line :: Parser String
line = many (satisfy (/='\n')) <* (many $ charP '\n')

manyP :: (Char -> Bool) -> Parser String
manyP f = many (satisfy f)

someP :: (Char -> Bool) -> Parser String
someP f = some (satisfy f)

oneOf :: [Char] -> Parser Char
oneOf lst = Parser $ \input ->
  case input of
    x:xs | elem x lst -> Just (xs, x)
    _                 -> Nothing

noneOf :: [Char] -> Parser Char
noneOf = notP . oneOf

notP :: Parser Char -> Parser Char 
notP (Parser p) = Parser $ \input ->
  case p input of
    Just (input', x) -> Nothing
    Nothing          -> Just (tail input, head input)

charP :: Char -> Parser Char
charP x = Parser $ \input ->
  case input of
    y:ys | y == x -> Just (ys, x)
    _             -> Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

eof :: Parser ()
eof = Parser $ \input -> case input of
  "" -> return ("", ())
  _  -> Nothing

