module PEG.GrammarParser
  ( parseGrammar
  , parseExpression
  ) where

import Control.Monad (void)
import Data.Char (chr, isAlpha, isAlphaNum, isHexDigit, isSpace)
import Numeric (readHex)
import Text.Parsec
  ( ParseError
  , Parsec
  , SourceName
  , anyChar
  , between
  , char
  , choice
  , eof
  , lookAhead
  , many
  , many1
  , noneOf
  , oneOf
  , option
  , optionMaybe
  , parse
  , satisfy
  , skipMany
  , string
  , try
  , unexpected
  , (<|>)
  )

import PEG.AST

type P = Parsec String ()

parseGrammar :: SourceName -> String -> Either ParseError Grammar
parseGrammar source = parse grammarP source

parseExpression :: SourceName -> String -> Either ParseError Expr
parseExpression source = parse (spaceConsumer *> expressionP <* eof) source

grammarP :: P Grammar
grammarP = do
  spaceConsumer
  rs <- many1 ruleP
  eof
  case rs of
    [] -> unexpected "at least one rule"
    firstRule : _ -> pure (Grammar (ruleName firstRule) rs)

ruleP :: P Rule
ruleP = do
  name <- lexeme identifier
  void (symbol "=")
  expr <- expressionP
  _ <- optionMaybe (symbol ";")
  spaceConsumer
  pure (Rule name expr)

expressionP :: P Expr
expressionP = smartChoice <$> sepBy1Local sequenceP (symbol "/")

sequenceP :: P Expr
sequenceP = smartSequence <$> many (notAtSequenceEnd *> prefixP)

prefixP :: P Expr
prefixP = andP <|> notP <|> suffixP
  where
    andP = And <$> (symbol "&" *> prefixP)
    notP = Not <$> (symbol "!" *> prefixP)

suffixP :: P Expr
suffixP = do
  p <- primaryP
  ops <- many (lexeme (oneOf "*+?"))
  pure (foldl applySuffix p ops)
  where
    applySuffix e '*' = Many e
    applySuffix e '+' = Some e
    applySuffix e '?' = Optional e
    applySuffix _ c   = error ("internal error: unknown suffix " ++ show c)

primaryP :: P Expr
primaryP = choice
  [ parens expressionP
  , try charClassP
  , literalP
  , anyP
  , epsilonWordP
  , nonTerminalP
  ]

literalP :: P Expr
literalP = do
  s <- lexeme quotedString
  pure (if null s then Epsilon else Literal s)

anyP :: P Expr
anyP = AnyChar <$ symbol "."

epsilonWordP :: P Expr
epsilonWordP = Epsilon <$ lexeme (try (string "epsilon") <|> string "eps" <|> string "ε")

nonTerminalP :: P Expr
nonTerminalP = do
  notRuleStart
  NonTerminal <$> lexeme identifier

charClassP :: P Expr
charClassP = lexeme $ do
  void (char '[')
  neg <- option False (True <$ char '^')
  atoms <- many1 classAtomP
  void (char ']')
  pure (CharClass neg atoms)

classAtomP :: P ClassAtom
classAtomP = try rangeP <|> (Single <$> classCharP)
  where
    rangeP = do
      a <- classCharP
      void (char '-')
      _ <- lookAhead (noneOf "]")
      b <- classCharP
      pure (Range a b)

classCharP :: P Char
classCharP = escapedChar <|> noneOf "]"

quotedString :: P String
quotedString = do
  q <- oneOf "\"'"
  many (escapedChar <|> noneOf [q, '\\']) <* char q

escapedChar :: P Char
escapedChar = do
  void (char '\\')
  c <- anyChar
  case c of
    'n'  -> pure '\n'
    'r'  -> pure '\r'
    't'  -> pure '\t'
    '\\' -> pure '\\'
    '\'' -> pure '\''
    '"'  -> pure '"'
    '['  -> pure '['
    ']'  -> pure ']'
    '-'  -> pure '-'
    'x'  -> hexEscape 2
    'u'  -> hexEscape 4
    other -> pure other

hexEscape :: Int -> P Char
hexEscape n = do
  ds <- countLocal n (satisfy isHexDigit)
  case readHex ds of
    [(v, "")] -> pure (chr v)
    _         -> unexpected "bad hexadecimal escape"

identifier :: P String
identifier = do
  first <- satisfy (\c -> isAlpha c || c == '_')
  rest <- many (satisfy (\c -> isAlphaNum c || c == '_' || c == '\''))
  pure (first : rest)

parens :: P a -> P a
parens = between (symbol "(") (symbol ")")

symbol :: String -> P String
symbol = lexeme . string

lexeme :: P a -> P a
lexeme p = p <* spaceConsumer

spaceConsumer :: P ()
spaceConsumer = skipMany (void (satisfy isSpace) <|> lineComment)

lineComment :: P ()
lineComment = do
  _ <- try (string "//" <|> string "#" <|> string "--")
  skipMany (noneOf "\r\n")
  pure ()

notAtSequenceEnd :: P ()
notAtSequenceEnd = do
  ended <- option False (True <$ lookAhead sequenceEnd)
  if ended then unexpected "end of sequence" else pure ()

sequenceEnd :: P ()
sequenceEnd = choice
  [ void eof
  , void (lookAhead (oneOf ")/;"))
  , void (try (lookAhead ruleStart))
  ]

ruleStart :: P ()
ruleStart = do
  _ <- identifier
  spaceConsumer
  void (char '=')

notRuleStart :: P ()
notRuleStart = do
  starts <- option False (True <$ try (lookAhead ruleStart))
  if starts then unexpected "start of the next rule" else pure ()

sepBy1Local :: P a -> P sep -> P [a]
sepBy1Local p sep = do
  x <- p
  xs <- many (sep *> p)
  pure (x : xs)

countLocal :: Int -> P a -> P [a]
countLocal n p
  | n <= 0    = pure []
  | otherwise = (:) <$> p <*> countLocal (n - 1) p
