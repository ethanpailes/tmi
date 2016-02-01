module Parse where
import Control.Exception
import Data.Typeable

import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe

data TypeError = TypeError String
  deriving( Typeable, Show )
instance Exception TypeError

data Q = Q String
    deriving( Eq, Ord, Show )
data Gamma = G String
    deriving( Eq, Ord, Show )
data Dir = L | R
    deriving( Eq, Ord, Show )

data Tm = Tm { name :: String
             , states :: [Q]
             , sigma :: [Char]
             , gamma :: [Gamma]
             , start :: String
             , accept :: String
             , reject :: String
             , delta ::
                  (Q, -- initial state
                   String) -> -- tape cell contents
                   Either Bool
                  (Q, -- next state
                   String, -- what to write to the tape
                   Dir) -- which direction to go
             }

initTM :: Tm
initTM = Tm {
    name = "Test TM"
  , states = [Q "qa", Q "qr", Q "q0"]
  , sigma = "01"
  , gamma = [G "0", G "1", G "B"]
  , start = "q0"
  , accept = "qa"
  , reject = "qr"
  , delta = \(_, _) -> Left False
  }

parseTuringMachine :: Parser Tm
parseTuringMachine = do
  tmConf <- parseJustConfig
  d <- parseDelta tmConf
  pure $ tmConf { delta = d }

parseJustConfig :: Parser Tm
parseJustConfig = do
  n <- parseName
  q <- parseStates
  sig <- parseSigma
  gam <- parseGamma
  startState <- parseStart
  acceptState <- parseAccept
  rejectState <- parseReject
  pure $ Tm {
      name = n
    , states = q
    , sigma = sig
    , gamma = gam
    , start = startState
    , accept = acceptState
    , reject = rejectState
    , delta = \(_, _) -> Left False -- dummy delta
    }


-- consumes space and comments, but not newlines
sc :: Parser ()
sc = L.space (void spaceNoNewline) lineComment blockComment
      where lineComment = L.skipLineComment "//"
            blockComment = L.skipBlockComment "/*" "*/"
            spaceNoNewline = satisfy $ liftA2 (&&) (/= '\n') isSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

line :: Parser a -> Parser a
line p = p >>= \res -> newline >> pure res

symbol :: String -> Parser String
symbol = L.symbol sc

restOfLine :: Parser String
restOfLine = many $ satisfy (/= '\n')

identifier :: Parser String
identifier = lexeme (many (alphaNumChar <|> punctuationChar))

parseName :: Parser String
parseName = line $ do
  _ <- lexeme $ symbol "NAME:"
  name <- restOfLine
  pure name

-- runParser parseName "test" "NAME: Ethan Pailes\n" == Right "Ethan Pailes"

parseStates :: Parser [Q]
parseStates = line $ do
  _ <- lexeme $ symbol "STATE:"
  rest <- restOfLine
  pure $ map Q $ words rest

parseSigma :: Parser [Char]
parseSigma = line $ do
  _ <- lexeme $ symbol "SIGMA:"
  rest <- restOfLine
  pure $
    if all (\xs -> length xs == 1) (words rest)
            then join (words rest)
            else throw $ TypeError "Sigma inputs must be single chars"


parseGamma :: Parser [Gamma]
parseGamma = line $ do
  _ <- lexeme $ symbol "GAMMA:"
  rest <- restOfLine
  pure $ if "B" `elem` (words rest)
            then map G $ words rest
            else throw $ TypeError "B must be an element of gamma."

parseStart :: Parser String
parseStart = line $ do
  _ <- lexeme $ symbol "START:"
  rest <- restOfLine
  pure rest
parseAccept :: Parser String
parseAccept = line $ do
  _ <- lexeme $ symbol "ACCEPT:"
  rest <- restOfLine
  pure rest
parseReject :: Parser String
parseReject = line $ do
  _ <- lexeme $ symbol "REJECT:"
  rest <- restOfLine
  pure rest

parseDelta :: Tm -> Parser ((Q, String) -> Either Bool (Q, String, Dir))
parseDelta tm = do
  _ <- lexeme (symbol "DELTA:") >> newline
  funLines <- someTill (parseDeltaLine tm) endDelta
  pure $ \ input ->
   let output = lookup input funLines
    in if output == Nothing then Left False else fromJust output

endDelta :: Parser String
endDelta = lexeme $ symbol "END"


parseDeltaLine :: Tm -> Parser ((Q, String), Either Bool (Q, String, Dir))
parseDeltaLine tm = do
    inputState <- identifier
    inputCell <- identifier
    outputState <- identifier
    outputCell <- optional identifier
    dir <- optional (char 'L' <|> char 'R')
    _ <- newline
    pure $ if Nothing /= outputCell && Nothing /= dir then
        ((Q inputState, inputCell),
          Right (Q outputState, fromJust outputCell, (conv . fromJust) dir))
      else ((Q inputState, inputCell), Left $
              if outputState == accept tm
                 then True
              else if outputState == reject tm
                 then False
              else throw $
                TypeError "Truncated line must enter accept or reject state")
      where conv 'L' = L
            conv 'R' = R

