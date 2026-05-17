module Main (main) where

import System.Exit (exitFailure)

import PEG.AST
import PEG.Codegen (generateModule)
import PEG.GrammarParser (parseGrammar)
import PEG.Matcher (acceptsGrammar)
import PEG.Validator (validateGrammar)

main :: IO ()
main = do
  testAnBn
  testAnBnCn
  testComplement
  testExpressionSyntax
  testOrderedChoice
  testLookaheadAndClasses
  testValidator
  testCodegen
  testExampleFiles
  putStrLn "All tests passed"

testAnBn :: IO ()
testAnBn = do
  let g = grammar "anbn" $ unlines
        [ "S = ANBN"
        , "ANBN = \"a\" ANBN \"b\" / \"a\" \"b\""
        ]
  assertAccepted g "ab"
  assertAccepted g "aabb"
  assertAccepted g "aaabbb"
  assertRejected g "aab"
  assertRejected g "aaabb"

testAnBnCn :: IO ()
testAnBnCn = do
  let g = grammar "anbncn" $ unlines
        [ "S = &(ANBN \"c\") As BNCN"
        , "ANBN = \"a\" ANBN \"b\" / \"a\" \"b\""
        , "As = \"a\" As / \"a\""
        , "BNCN = \"b\" BNCN \"c\" / \"b\" \"c\""
        ]
  assertAccepted g "abc"
  assertAccepted g "aabbcc"
  assertAccepted g "aaabbbccc"
  assertRejected g "aabbc"
  assertRejected g "aabbbccc"

testComplement :: IO ()
testComplement = do
  let g = grammar "not_anbn" $ unlines
        [ "S = !(ANBN EOF) ANYe"
        , "ANBN = \"a\" ANBN \"b\" / \"a\" \"b\""
        , "ALPH = \"a\" / \"b\" / \"c\""
        , "ANYe = ALPH ANYe / \"\""
        , "EOF = !."
        ]
  assertRejected g "ab"
  assertRejected g "aabb"
  assertAccepted g "aab"
  assertAccepted g "ac"
  assertAccepted g ""

testExpressionSyntax :: IO ()
testExpressionSyntax = do
  let g = grammar "expr" $ unlines
        [ "# comments may start with #"
        , "Expr = Term (\"+\" Term)*"
        , "Term = Factor (\"*\" Factor)*"
        , "Factor = Number / \"(\" Expr \")\""
        , "Number = [0-9]+"
        ]
  assertAccepted g "2"
  assertAccepted g "2+3*4"
  assertAccepted g "(2+3)*4"
  assertRejected g "2+"
  assertRejected g "(2+3"

testOrderedChoice :: IO ()
testOrderedChoice = do
  -- PEG choice is ordered: the first branch "a" succeeds on input "ab",
  -- so the parser does not try "ab" afterwards; end-of-input then rejects.
  let g = grammar "ordered-choice" "S = \"a\" / \"ab\""
  assertAccepted g "a"
  assertRejected g "ab"

testLookaheadAndClasses :: IO ()
testLookaheadAndClasses = do
  let g = grammar "lookahead" $ unlines
        [ "S = &([a-z]+ EOF) !\"bad\" [a-z]+"
        , "EOF = !."
        ]
  assertAccepted g "abc"
  assertRejected g "bad"
  assertRejected g "abc1"

testValidator :: IO ()
testValidator = do
  assertInvalid "duplicate rule" $ unlines
    [ "S = \"a\""
    , "S = \"b\""
    ]
  assertInvalid "undefined nonterminal" "S = Missing"
  assertInvalid "direct left recursion" "S = S \"a\" / \"a\""
  assertInvalid "indirect left recursion" $ unlines
    [ "S = A"
    , "A = S / \"a\""
    ]
  assertInvalid "zero-width repetition" "S = (\"\")*"

testCodegen :: IO ()
testCodegen = do
  let g = grammar "codegen" $ unlines
        [ "S = \"a\" S \"b\" / \"a\" \"b\""
        ]
      generated = generateModule "GeneratedPEG" g
  assertContains "generated module header" "module GeneratedPEG (parse, start) where" generated
  assertContains "generated start parser" "start = p_s" generated
  assertContains "generated named parser" "p_s = named \"S\"" generated

testExampleFiles :: IO ()
testExampleFiles = do
  checkFile "examples/identifier.peg" ["user_1", "_tmp", "ifx"] ["if", "1abc", "has-dash"]
  checkFile "examples/number.peg" ["0", "+42", "-3.14"] ["", "3.", ".5", "1.2.3"]
  checkFile "examples/hex_color.peg" ["#0F8", "#ff00AA"] ["#abcd", "ff00AA", "#xyz"]
  checkFile "examples/iso_date_like.peg" ["2026-05-14", "1999-12-31"] ["2026-13-14", "2026-00-01", "2026-05-40"]
  checkFile "examples/ipv4_like.peg" ["192.168.0.1", "255.255.255.255", "0.0.0.0"] ["256.0.0.1", "01.2.3.4", "1.2.3"]
  checkFile "examples/email_like.peg" ["user.name+tag@example.com", "a_b@test42.org"] ["user@example", "@example.com", "user@@example.com"]
  checkFile "examples/url_like.peg" ["http://example.com", "https://example.com/a/b"] ["ftp://example.com", "https://localhost", "https:/example.com"]
  checkFile "examples/balanced_parentheses.peg" ["", "()", "(()())"] ["(", "())", "(()"]

grammar :: String -> String -> Grammar
grammar name source = case parseGrammar name source of
  Left err -> error (show err)
  Right g -> case validateGrammar g of
    [] -> g
    errs -> error (unlines errs)

assertAccepted :: Grammar -> String -> IO ()
assertAccepted g s = assertResult True g s

assertRejected :: Grammar -> String -> IO ()
assertRejected g s = assertResult False g s

assertResult :: Bool -> Grammar -> String -> IO ()
assertResult expected g s = case acceptsGrammar g s of
  Left err -> do
    putStrLn ("Runtime error on " ++ show s ++ ": " ++ err)
    exitFailure
  Right actual
    | actual == expected -> pure ()
    | otherwise -> do
        putStrLn ("For input " ++ show s ++ " expected " ++ show expected ++ ", got " ++ show actual)
        exitFailure

assertInvalid :: String -> String -> IO ()
assertInvalid label source =
  case parseGrammar label source of
    Left _ -> pure ()
    Right g ->
      case validateGrammar g of
        [] -> do
          putStrLn ("Expected invalid grammar for " ++ label)
          exitFailure
        _ -> pure ()

checkFile :: FilePath -> [String] -> [String] -> IO ()
checkFile path accepted rejected = do
  source <- readFile path
  let g = grammar path source
  mapM_ (assertAccepted g) accepted
  mapM_ (assertRejected g) rejected

assertContains :: String -> String -> String -> IO ()
assertContains label needle haystack =
  if needle `isSubstringOf` haystack
    then pure ()
    else do
      putStrLn ("Missing " ++ show needle ++ " in " ++ label)
      exitFailure

isSubstringOf :: Eq a => [a] -> [a] -> Bool
isSubstringOf [] _ = True
isSubstringOf _ [] = False
isSubstringOf needle haystack@(_:rest) =
  needle `isPrefixOf` haystack || needle `isSubstringOf` rest

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
