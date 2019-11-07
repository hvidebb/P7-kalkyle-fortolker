{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser (parseString, parseStringM) where
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec

import Process

import Data.String
import Data.Either

instance IsString Env where
  fromString = either (error . show) id . parseString 

instance IsString (Maybe Env) where
  fromString = parseStringM

instance IsString (Either String Env) where
  fromString = parseString

parseStringM :: String -> Maybe Env
parseStringM = either (const Nothing) Just . parse parseExpr ""

parseString :: String -> Either String Env
parseString = either (Left . show) (Right . id) . parse parseExpr ""

parseExpr :: Parser (Env)
parseExpr = par >>= (\x -> eof >> return x) >>= return . unpar >>= return . SRChain

name :: Parser String
name = many1 letter

par :: Parser Process
par = SRPar <$> sepBy process (spaces >> char '|' >> spaces)

process :: Parser Process
process = nil <|> rdrop <|> prefix <|> between (char '(') (char ')') par

rdrop :: Parser Process
rdrop = between (char '\'') (char '\'') name >>= return . SRDrop

nil :: Parser Process
nil = char '0' >> spaces >> return SRNil

prefix :: Parser Process
prefix = (name >>= \n -> (lift n <|> recv n)) <*> next
  where lift n = between (char '{') (char '}') name >>= return . SRLift n
        recv n = between (char '(') (char ')') name >>= return . SRRecv n
        next   = option SRNil (char '.' >> process)

