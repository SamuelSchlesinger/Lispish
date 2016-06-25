import System.Environment
import Lisp.Evaluator
import Lisp.Parser

preprocess :: FilePath -> IO [A]
preprocess path = do
  str <- readFile path
  case parse sfile path str of
    Left err -> print err >> return []
    Right defs -> do let includes = map (\(_ := v) -> v) $ getIncludes defs
                     let syms = includes >>= symbols
                     included <- mapM preprocess syms
                     return (defs ++ concat included)

main = do
  (f:_) <- getArgs
{-  str <- readFile f
  case parse sfile f str of
    Left err -> print err
    Right defs -> case runLisp defs of
      Left err -> print err
      Right res -> print res >> (print $ depth res)-}
  defs <- preprocess f
  case runLisp defs of
    Left err -> print err
    Right res -> print res >> (print $ depth res)
