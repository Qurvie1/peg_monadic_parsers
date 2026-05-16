module PEG.AST
  ( Name
  , Grammar(..)
  , Rule(..)
  , Expr(..)
  , ClassAtom(..)
  , smartSequence
  , smartChoice
  , referencedNames
  , prettyGrammar
  , prettyRule
  , prettyExpr
  ) where

import Data.List (intercalate, nub)

type Name = String

data Grammar = Grammar
  { startRule :: Name
  , rules     :: [Rule]
  } deriving (Eq, Show)

-- A = e
data Rule = Rule
  { ruleName :: Name
  , ruleExpr :: Expr
  } deriving (Eq, Show)

data Expr
  = Epsilon
  | Literal String
  | AnyChar
  | CharClass Bool [ClassAtom]
  | NonTerminal Name
  | Sequence [Expr]
  | Choice [Expr]
  | And Expr
  | Not Expr
  | Many Expr
  | Some Expr
  | Optional Expr
  deriving (Eq, Show)

data ClassAtom
  = Single Char
  | Range Char Char
  deriving (Eq, Show)

smartSequence :: [Expr] -> Expr
smartSequence []  = Epsilon
smartSequence [x] = x
smartSequence xs  = Sequence xs

smartChoice :: [Expr] -> Expr
smartChoice []  = error "internal error: empty PEG choice"
smartChoice [x] = x
smartChoice xs  = Choice xs

referencedNames :: Expr -> [Name]
referencedNames expr = nub (go expr)
  where
    go Epsilon          = []
    go (Literal _)      = []
    go AnyChar          = []
    go (CharClass _ _)  = []
    go (NonTerminal n)  = [n]
    go (Sequence xs)    = concatMap go xs
    go (Choice xs)      = concatMap go xs
    go (And e)          = go e
    go (Not e)          = go e
    go (Many e)         = go e
    go (Some e)         = go e
    go (Optional e)     = go e

prettyGrammar :: Grammar -> String
prettyGrammar g = unlines (map prettyRule (rules g))

prettyRule :: Rule -> String
prettyRule (Rule n e) = n ++ " = " ++ prettyExpr e

prettyExpr :: Expr -> String
prettyExpr Epsilon = "\"\""
prettyExpr (Literal s) = show s
prettyExpr AnyChar = "."
prettyExpr (CharClass neg atoms) = "[" ++ (if neg then "^" else "") ++ concatMap prettyAtom atoms ++ "]"
prettyExpr (NonTerminal n) = n
prettyExpr (Sequence xs) = unwords (map withParensForSeq xs)
prettyExpr (Choice xs) = intercalate " / " (map withParensForChoice xs)
prettyExpr (And e) = "&" ++ withParensForPrefix e
prettyExpr (Not e) = "!" ++ withParensForPrefix e
prettyExpr (Many e) = withParensForSuffix e ++ "*"
prettyExpr (Some e) = withParensForSuffix e ++ "+"
prettyExpr (Optional e) = withParensForSuffix e ++ "?"

prettyAtom :: ClassAtom -> String
prettyAtom (Single c) = escapeClassChar c
prettyAtom (Range a b) = escapeClassChar a ++ "-" ++ escapeClassChar b

escapeClassChar :: Char -> String
escapeClassChar '\\' = "\\\\"
escapeClassChar ']'  = "\\]"
escapeClassChar '-'  = "\\-"
escapeClassChar c    = [c]

withParensForSeq :: Expr -> String
withParensForSeq e@(Choice _) = "(" ++ prettyExpr e ++ ")"
withParensForSeq e           = prettyExpr e

withParensForChoice :: Expr -> String
withParensForChoice e@(Choice _)   = "(" ++ prettyExpr e ++ ")"
withParensForChoice e             = prettyExpr e

withParensForPrefix :: Expr -> String
withParensForPrefix e@(Choice _)   = "(" ++ prettyExpr e ++ ")"
withParensForPrefix e@(Sequence _) = "(" ++ prettyExpr e ++ ")"
withParensForPrefix e             = prettyExpr e

withParensForSuffix :: Expr -> String
withParensForSuffix e@(Literal _)     = prettyExpr e
withParensForSuffix e@AnyChar         = prettyExpr e
withParensForSuffix e@(CharClass _ _) = prettyExpr e
withParensForSuffix e@(NonTerminal _) = prettyExpr e
withParensForSuffix e@Epsilon         = prettyExpr e
withParensForSuffix e                 = "(" ++ prettyExpr e ++ ")"
