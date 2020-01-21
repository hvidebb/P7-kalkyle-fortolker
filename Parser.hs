module Parser where
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec hiding ((<|>))
import Control.Applicative
import Data.Function (on)

import Process

import Data.String
import Data.Either

testParse :: String -> [Process]
testParse = either (error . show) id . parseProcess

parseProcessM :: String -> Maybe [Process]
parseProcessM = either (const Nothing) Just . parseProcess

parseProcess :: String -> Either ParseError [Process]
parseProcess = parse (par >>= \x -> eof >> return x) ""

name :: Parser String
name = many1 lower

process :: Parser [Process]
process = choice [ (:[]) <$> prefix, (:[]) <$> rdrop, (:[]) <$> macro,
                   between (char '(') (char ')') par, nil ]

par :: Parser [Process]
par = concat <$> sepBy process (spaces >> char '|' >> spaces)

nil :: Parser [Process]
nil = char '0' >> spaces >> return []

rdrop :: Parser Process
rdrop = RDrop <$> between (char '\'') (char '\'') name

macro :: Parser Process
macro = Macro <$> many1 upper

prefix :: Parser Process
prefix = name >>= \n -> recv n <|> lift n
  where recv n = Recv n <$> between (char '(') (char ')') name <*> next
        lift n = Lift n <$> between (char '{') (char '}') par
        next   = option [] $ char '.' >> process
