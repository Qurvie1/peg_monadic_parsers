module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import PEG.AST
import PEG.Codegen (generateModule)
import PEG.GrammarParser (parseGrammar)
import PEG.Matcher (acceptsGrammar)
import PEG.Validator (validateGrammar)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putStrLn usage
    ["--ast", grammarFile] -> do
      g <- loadGrammar grammarFile Nothing
      putStr (prettyGrammar g)
    ["--emit-hs", outFile, grammarFile] -> do
      g <- loadGrammar grammarFile Nothing
      writeFile outFile (generateModule "GeneratedPEG" g)
      putStrLn ("written " ++ outFile)
    [grammarFile, input] -> runCheck grammarFile Nothing input
    [grammarFile, input, "--start", start] -> runCheck grammarFile (Just start) input
    [grammarFile, input, "-s", start] -> runCheck grammarFile (Just start) input
    _ -> do
      hPutStrLn stderr usage
      exitWith (ExitFailure 1)

runCheck :: FilePath -> Maybe Name -> String -> IO ()
runCheck grammarFile mStart input = do
  g <- loadGrammar grammarFile mStart
  case acceptsGrammar g input of
    Left err -> do
      hPutStrLn stderr ("runtime error: " ++ err)
      exitWith (ExitFailure 1)
    Right True -> putStrLn "ACCEPTED"
    Right False -> do
      putStrLn "REJECTED"
      exitWith (ExitFailure 2)

loadGrammar :: FilePath -> Maybe Name -> IO Grammar
loadGrammar grammarFile mStart = do
  source <- readFile grammarFile
  case parseGrammar grammarFile source of
    Left err -> do
      hPutStrLn stderr (show err)
      exitWith (ExitFailure 1)
    Right g0 -> do
      let g = maybe g0 (\s -> g0 { startRule = s }) mStart
          errors = validateGrammar g
      case errors of
        [] -> pure g
        _  -> do
          mapM_ (hPutStrLn stderr) errors
          exitWith (ExitFailure 1)

usage :: String
usage = unlines
  [ "peg-check - PEG membership checker"
  , ""
  , "Usage:"
  , "  peg-check GRAMMAR_FILE INPUT_STRING"
  , "  peg-check GRAMMAR_FILE INPUT_STRING --start RULE"
  , "  peg-check --ast GRAMMAR_FILE"
  , "  peg-check --emit-hs OUT.hs GRAMMAR_FILE"
  , ""
  , "Exit codes: 0 = accepted, 2 = rejected, 1 = invalid grammar/runtime/CLI error."
  ]
