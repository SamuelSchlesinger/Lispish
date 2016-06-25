import System.Environment
import Lisp.Evaluator
import Lisp.Parser

main = do
  (f:_) <- getArgs
  str <- readFile f 
  let defs = parse sfile f str
  case defs of
    Left err -> print err
    Right ds -> case runLisp ds of
                  Left err -> print err
                  Right res -> print res >> (print $ depth res)
