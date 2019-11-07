{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}


module Parser (parseProcess, parseProcessM, testParse) where
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec

import Process

import Data.String
import Data.Either

instance IsString (SRProcess String) where
  fromString = either (error . show) id . parseProcess 

testParse :: String -> Process
testParse = either (error . show) id . parseProcess

parseProcessM :: String -> Maybe Process
parseProcessM = either (const Nothing) Just . parseProcess

parseProcess :: String -> Either ParseError Process
parseProcess = parse (par >>= \x -> eof >> return x) ""

name :: Parser String
name = many1 lower

process :: Parser Process
process = choice [nil, rdrop,
                  prefix, macro, between (char '(') (char ')') par]

par :: Parser Process
par = cleanup <$> sepBy process (spaces >> char '|' >> spaces)
  where cleanup xs     = foldr1 `flip` xs $ \case
          SRNil -> id
          x     -> \case
            SRNil -> x
            y     -> (x <> y)

rdrop :: Parser Process
rdrop = between (char '\'') (char '\'') name >>= return . SRDrop

nil :: Parser Process
nil = char '0' >> spaces >> return SRNil

prefix :: Parser Process
prefix = (name >>= \n -> (lift n <|> recv n)) <*> next
  where lift n = between (char '{') (char '}') process >>= return . SRLift n
        recv n = between (char '(') (char ')') name >>= return . SRRecv n
        next   = option SRNil (char '.' >> process)

macro :: Parser Process
macro = many1 upper >>= return . SRMacro
