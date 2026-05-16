module PEG.Codegen
  ( generateModule
  ) where

import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate)

import PEG.AST

generateModule :: String -> Grammar -> String
generateModule moduleName g = unlines $
  [ "{-# OPTIONS_GHC -Wall #-}"
  , "module " ++ moduleName ++ " (parse, start) where"
  , ""
  , "import PEG.Runtime"
  , ""
  , "parse :: String -> Either String Bool"
  , "parse = accepts start"
  , ""
  , "start :: Parser ()"
  , "start = " ++ hsName (startRule g)
  , ""
  ] ++ concatMap renderRule (rules g)
  where
    renderRule (Rule n e) =
      [ hsName n ++ " :: Parser ()"
      , hsName n ++ " = named " ++ show n ++ " $ " ++ renderExpr e
      , ""
      ]

renderExpr :: Expr -> String
renderExpr expr = case expr of
  Epsilon -> "pure ()"
  Literal s -> "literal " ++ show s
  AnyChar -> "() <$ anyChar"
  CharClass neg atoms -> "charClass " ++ show neg ++ " " ++ renderAtoms atoms
  NonTerminal n -> hsName n
  Sequence xs -> renderSequence xs
  Choice xs -> "choose [" ++ intercalate ", " (map renderExpr xs) ++ "]"
  And e -> "look (" ++ renderExpr e ++ ")"
  Not e -> "neg (" ++ renderExpr e ++ ")"
  Many e -> "manyP (" ++ renderExpr e ++ ")"
  Some e -> "someP (" ++ renderExpr e ++ ")"
  Optional e -> "optionalP (" ++ renderExpr e ++ ")"

renderSequence :: [Expr] -> String
renderSequence [] = "pure ()"
renderSequence xs = intercalate " *> " (map parenthesized xs)
  where
    parenthesized e@(Choice _) = "(" ++ renderExpr e ++ ")"
    parenthesized e@(Sequence _) = "(" ++ renderExpr e ++ ")"
    parenthesized e = "(" ++ renderExpr e ++ ")"

renderAtoms :: [ClassAtom] -> String
renderAtoms atoms = "[" ++ intercalate ", " (map renderAtom atoms) ++ "]"

renderAtom :: ClassAtom -> String
renderAtom (Single c) = "Single " ++ show c
renderAtom (Range a b) = "Range " ++ show a ++ " " ++ show b

hsName :: Name -> String
hsName n = "p_" ++ concatMap sanitize n
  where
    sanitize c
      | isAlphaNum c = [toLower c]
      | otherwise    = "_"
