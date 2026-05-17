module PEG.Runtime
  ( Parser
  , accepts
  , runParserFromStart
  , literal
  , anyChar
  , satisfy
  , charClass
  , endOfInput
  , look
  , neg
  , choose
  , manyP
  , someP
  , optionalP
  , named
  , getPosition
  , failP
  , ClassAtom(..)
  ) where

import Control.Applicative (Alternative(..))
import qualified Data.Map.Strict as M

import PEG.AST (ClassAtom(..))

data RunState = RunState
  { input    :: String
  , position :: Int
  , memo     :: M.Map MemoKey MemoEntry
  } deriving (Eq, Show)

type MemoKey = (String, Int)

data MemoEntry
  = InProgress
  | Done Bool Int
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: RunState -> Either String (Maybe a, RunState) }

instance Functor Parser where
  fmap f p = p >>= (pure . f)

instance Applicative Parser where
  pure x = Parser $ \st -> Right (Just x, st)
  pf <*> px = do
    f <- pf
    x <- px
    pure (f x)

instance Monad Parser where
  Parser p >>= f = Parser $ \st0 -> do
    (mx, st1) <- p st0
    case mx of
      Nothing -> Right (Nothing, st0 { memo = memo st1 })
      Just x -> do
        (my, st2) <- runParser (f x) st1
        case my of
          Nothing -> Right (Nothing, st0 { memo = memo st2 })
          Just y  -> Right (Just y, st2)

instance Alternative Parser where
  empty = failP
  Parser p <|> Parser q = Parser $ \st0 -> do
    (mx, st1) <- p st0
    case mx of
      Just _  -> Right (mx, st1)
      Nothing -> q (st0 { memo = memo st1 })

failP :: Parser a
failP = Parser $ \st -> Right (Nothing, st)

getPosition :: Parser Int
getPosition = Parser $ \st -> Right (Just (position st), st)


runParserFromStart :: Parser a -> String -> Either String (Maybe a, Int)
runParserFromStart p s = do
  (mx, st) <- runParser p (RunState s 0 M.empty)
  pure (mx, position st)

accepts :: Parser () -> String -> Either String Bool
accepts p s = do
  (mx, _st) <- runParser (p *> endOfInput) (RunState s 0 M.empty)
  pure (case mx of
    Just () -> True
    Nothing -> False)

literal :: String -> Parser ()
literal needle = Parser $ \st ->
  let pos = position st
      rest = drop pos (input st)
  in if needle `prefixOf` rest
       then Right (Just (), st { position = pos + length needle })
       else Right (Nothing, st)

anyChar :: Parser Char
anyChar = satisfy (const True)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \st ->
  case drop (position st) (input st) of
    [] -> Right (Nothing, st)
    c:_ | predicate c -> Right (Just c, st { position = position st + 1 })
        | otherwise   -> Right (Nothing, st)

charClass :: Bool -> [ClassAtom] -> Parser ()
charClass isNegated atoms = do
  _ <- satisfy predicate
  pure ()
  where
    predicate c = let inside = any (contains c) atoms
                  in if isNegated then not inside else inside

    contains c (Single x) = c == x
    contains c (Range a b) = a <= c && c <= b

endOfInput :: Parser ()
endOfInput = Parser $ \st ->
  if position st == length (input st)
     then Right (Just (), st)
     else Right (Nothing, st)

look :: Parser a -> Parser ()
look (Parser p) = Parser $ \st0 -> do
  (mx, st1) <- p st0
  case mx of
    Just _  -> Right (Just (), st0 { memo = memo st1 })
    Nothing -> Right (Nothing, st0 { memo = memo st1 })

neg :: Parser a -> Parser ()
neg (Parser p) = Parser $ \st0 -> do
  (mx, st1) <- p st0
  case mx of
    Just _  -> Right (Nothing, st0 { memo = memo st1 })
    Nothing -> Right (Just (), st0 { memo = memo st1 })

choose :: [Parser ()] -> Parser ()
choose [] = empty
choose [p] = p
choose (p:ps) = p <|> choose ps

manyP :: Parser () -> Parser ()
manyP p = go
  where
    go = branch <|> pure ()
    branch = do
      before <- getPosition
      p
      after <- getPosition
      if after == before
        then Parser $ \_ -> Left "internal error: repetition parser consumed no input"
        else go

someP :: Parser () -> Parser ()
someP p = p *> manyP p

optionalP :: Parser () -> Parser ()
optionalP p = p <|> pure ()

named :: String -> Parser () -> Parser ()
named name (Parser body) = Parser $ \st0 -> do
  let key = (name, position st0)
  case M.lookup key (memo st0) of
    Just InProgress -> Left $ "left recursion or cyclic zero-width call at rule " ++ show name
      ++ " and position " ++ show (position st0)
    Just (Done False _) -> Right (Nothing, st0)
    Just (Done True endPos) -> Right (Just (), st0 { position = endPos })
    Nothing -> do
      let stMarked = st0 { memo = M.insert key InProgress (memo st0) }
      (mx, st1) <- body stMarked
      let entry = case mx of
            Just () -> Done True (position st1)
            Nothing -> Done False (position st0)
          stMemoized = st1 { memo = M.insert key entry (memo st1) }
      case entry of
        Done True endPos -> Right (Just (), stMemoized { position = endPos })
        Done False _     -> Right (Nothing, stMemoized { position = position st0 })
        InProgress       -> error "impossible: memo entry just computed"

prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _ = True
prefixOf _ [] = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys
