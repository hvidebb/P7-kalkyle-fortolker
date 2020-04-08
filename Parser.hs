module Parser (parse, testparse, testparseName) where
import RhoProcess
import ParserCompinators
import Data.Char
import Control.Applicative
import Control.Monad

import Data.Function (on)

parse :: String -> Maybe Process
parse = fmap snd . runParser (parsePara <* eof)

testparse :: String -> Process
testparse s = case parse s of
  Nothing -> error "Could not parse process"
  Just p  -> p

testparseName :: String -> Name
testparseName s = case fmap snd $ (runParser (parseName <* eof) s) of
  Nothing -> error "Could not parse name"
  Just n  -> n

parseName :: Parser Name
parseName = Name `flip` Nothing <$>
            some (satisfy isLetter <|> satisfy (== '\''))

parsePara :: Parser Process
parsePara = mconcat <$> sepMany (parseProcess <* spaces)
                                  (charP '|' *> spaces)

parseProcess :: Parser Process
parseProcess = choice
  [ ProcessT . return <$> parsePrefix
  , charP '0' >> return mempty
  , ProcessT . return . Drop <$> parseName
  , ProcessT . return . Scope <$> between (stringP "[") (stringP "]") parsePara
  , between (charP '(' <* spaces) (charP ')' <* spaces) parsePara
  ]

parsePrefix :: Parser Operation
parsePrefix = parseName >>= \n -> recv n <|> lift n
  where recv n = Recv n <$> between (stringP "(") (stringP ")") parseName
                          <*> option mempty (charP '.' >> parseProcess)
        lift n = Lift n <$> between (stringP "{") (stringP "}") parseProcess
