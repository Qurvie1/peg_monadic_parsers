module PEG.Validator
  ( validateGrammar
  , ruleMap
  , duplicateNames
  , zeroProgress
  , leftCallGraph
  ) where

import Data.List (intercalate, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import PEG.AST

-- | Validate the grammar before running the recursive-descent interpreter.
--
-- The checks are deliberately conservative: whenever an expression may succeed
-- without consuming input, putting it under * or + is rejected, because a greedy
-- repetition of such an expression can loop forever.
validateGrammar :: Grammar -> [String]
validateGrammar g = concat
  [ duplicateErrors
  , startErrors
  , undefinedErrors
  , leftRecursionErrors
  , repetitionErrors
  ]
  where
    names = map ruleName (rules g)
    dups = duplicateNames names
    rm = ruleMap g
    defined = M.keysSet rm

    duplicateErrors =
      [ "duplicate rule definitions: " ++ intercalate ", " dups | not (null dups) ]

    startErrors =
      [ "start rule is not defined: " ++ startRule g | startRule g `S.notMember` defined ]

    undefinedRefs =
      [ (ruleName r, ref)
      | r <- rules g
      , ref <- referencedNames (ruleExpr r)
      , ref `S.notMember` defined
      ]

    undefinedErrors =
      [ "undefined nonterminal " ++ show ref ++ " referenced from rule " ++ show from
      | (from, ref) <- undefinedRefs
      ]

    leftRecursive = [ n | n <- M.keys rm, n `S.member` reachableFrom n (leftCallGraph g) ]

    leftRecursionErrors =
      [ "left recursion detected in rules: " ++ intercalate ", " leftRecursive
      | not (null leftRecursive)
      ]

    zp = zeroProgress g
    badRepetitions =
      [ ruleName r ++ ": " ++ prettyExpr e
      | r <- rules g
      , e <- repeatedSubexpressions (ruleExpr r)
      , maySucceedWithoutConsuming zp e
      ]

    repetitionErrors =
      [ "repetition of an expression that may consume no input: " ++ intercalate "; " badRepetitions
      | not (null badRepetitions)
      ]

ruleMap :: Grammar -> M.Map Name Expr
ruleMap = M.fromList . map (\r -> (ruleName r, ruleExpr r)) . rules

duplicateNames :: [Name] -> [Name]
duplicateNames names =
  [ x
  | x : _ : _ <- groupSorted (sort names)
  ]
  where
    groupSorted [] = []
    groupSorted (x:xs) = let (same, rest) = span (== x) xs in (x:same) : groupSorted rest

-- | Fixed-point approximation: True means that an expression can succeed while
-- leaving the input position unchanged on at least one input.  It is used to
-- protect greedy repetitions and to approximate first nonterminal calls.
zeroProgress :: Grammar -> M.Map Name Bool
zeroProgress g = fixpoint initial
  where
    rm = ruleMap g
    initial = M.map (const False) rm
    step env = M.map (maySucceedWithoutConsuming env) rm
    fixpoint env =
      let env' = step env
      in if env' == env then env else fixpoint env'

maySucceedWithoutConsuming :: M.Map Name Bool -> Expr -> Bool
maySucceedWithoutConsuming env expr = case expr of
  Epsilon         -> True
  Literal s       -> null s
  AnyChar         -> False
  CharClass _ _   -> False
  NonTerminal n   -> M.findWithDefault False n env
  Sequence xs     -> all (maySucceedWithoutConsuming env) xs
  Choice xs       -> any (maySucceedWithoutConsuming env) xs
  -- Predicates never consume input.  They are conservative here: if they are
  -- used under repetition, there exists a risk of an infinite loop.
  And _           -> True
  Not _           -> True
  Many _          -> True
  Some e          -> maySucceedWithoutConsuming env e
  Optional _      -> True

leftCallGraph :: Grammar -> M.Map Name (S.Set Name)
leftCallGraph g = M.map (leftCalls (zeroProgress g)) (ruleMap g)

leftCalls :: M.Map Name Bool -> Expr -> S.Set Name
leftCalls zp expr = case expr of
  Epsilon       -> S.empty
  Literal _     -> S.empty
  AnyChar       -> S.empty
  CharClass _ _ -> S.empty
  NonTerminal n -> S.singleton n
  Choice xs     -> S.unions (map (leftCalls zp) xs)
  Sequence xs   -> seqCalls xs
  And e         -> leftCalls zp e
  Not e         -> leftCalls zp e
  Many e        -> leftCalls zp e
  Some e        -> leftCalls zp e
  Optional e    -> leftCalls zp e
  where
    seqCalls [] = S.empty
    seqCalls (x:xs)
      | maySucceedWithoutConsuming zp x = leftCalls zp x `S.union` seqCalls xs
      | otherwise                       = leftCalls zp x

reachableFrom :: Name -> M.Map Name (S.Set Name) -> S.Set Name
reachableFrom start graph = go S.empty (S.toList (M.findWithDefault S.empty start graph))
  where
    go seen [] = seen
    go seen (x:xs)
      | x `S.member` seen = go seen xs
      | otherwise = go (S.insert x seen) (S.toList (M.findWithDefault S.empty x graph) ++ xs)

repeatedSubexpressions :: Expr -> [Expr]
repeatedSubexpressions expr = case expr of
  Many e       -> e : repeatedSubexpressions e
  Some e       -> e : repeatedSubexpressions e
  Sequence xs  -> concatMap repeatedSubexpressions xs
  Choice xs    -> concatMap repeatedSubexpressions xs
  And e        -> repeatedSubexpressions e
  Not e        -> repeatedSubexpressions e
  Optional e   -> repeatedSubexpressions e
  Epsilon      -> []
  Literal _    -> []
  AnyChar      -> []
  CharClass _ _ -> []
  NonTerminal _ -> []
