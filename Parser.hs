module Parser (parse) where
import RhoProcess
import ParserCompinators
import Data.Char
import Control.Applicative
import Control.Monad

import Data.Function (on)

parse :: String -> Maybe Process
parse = fmap snd . runParser (parsePara <* eof)

parseName :: IsRhoName a => Parser a
parseName = quote `flip` Nothing <$>
            some (satisfy isLetter <|> satisfy (== '\''))

parsePara :: IsRhoName a => Parser (ProcessT a)
parsePara = mconcat <$> sepMany (parseProcess <* spaces)
                                  (charP '|' *> spaces)

parseProcess :: IsRhoName a => Parser (ProcessT a)
parseProcess = choice
  [ ProcessT . return <$> parsePrefix
  , charP '0' >> return mempty
  , ProcessT . return . Drop <$> parseName
  , ProcessT . return . Scope <$> between (stringP "[") (stringP "]") parsePara
  , between (charP '(' <* spaces) (charP ')' <* spaces) parsePara
  ]

parsePrefix :: IsRhoName a => Parser (OperationT a)
parsePrefix = parseName >>= \n -> recv n <|> lift n
  where recv n = Recv n <$> between (stringP "(") (stringP ")") parseName
                          <*> option mempty (charP '.' >> parseProcess)
        lift n = Lift n <$> between (stringP "{") (stringP "}") parseProcess
