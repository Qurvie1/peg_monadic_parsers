module PEG.Matcher
  ( compileGrammar
  , compileExpression
  , acceptsGrammar
  ) where

import qualified Data.Map.Strict as M

import PEG.AST
import PEG.Runtime
import PEG.Validator (ruleMap)

-- | Compile the start rule of a grammar into a monadic parser.
compileGrammar :: Grammar -> Parser ()
compileGrammar g = compileExpression g (NonTerminal (startRule g))

-- | Interpret a parsing expression as a monadic parser.  The interpreter is
-- syntax-directed: every PEG constructor maps to a parser combinator.
compileExpression :: Grammar -> Expr -> Parser ()
compileExpression g expr = go expr
  where
    rm = ruleMap g

    go Epsilon = pure ()
    go (Literal s) = literal s
    go AnyChar = () <$ anyChar
    go (CharClass isNegated atoms) = charClass isNegated atoms
    go (NonTerminal n) = case M.lookup n rm of
      Nothing -> failP
      Just e  -> named n (go e)
    go (Sequence xs) = mapM_ go xs
    go (Choice xs) = choose (map go xs)
    go (And e) = look (go e)
    go (Not e) = neg (go e)
    go (Many e) = manyP (go e)
    go (Some e) = someP (go e)
    go (Optional e) = optionalP (go e)

-- | Full membership check.  The parser must match the whole input.
acceptsGrammar :: Grammar -> String -> Either String Bool
acceptsGrammar g = accepts (compileGrammar g)
