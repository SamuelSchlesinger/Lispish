module Lisp.Evaluator (
  evaluate,
  runLisp,
  getIncludes
) where

-- Maybe works

import Data.List
import Lisp.Parser (S(..), A(..))

evaluate :: [A] -> S -> Either String S 

evaluate context T = return T
evaluate context F = return F
evaluate context (Number n) = return $ Number n
evaluate context (Symbol v) = case find (\(v' :=  x) -> v' == v) context of
                                Nothing -> Right $ Symbol v
                                Just (_ := x) -> Right x

evaluate context (Quote e) = return e
evaluate context (Lambda vs e) = case evaluate ((zipWith (\x y -> x := Symbol y) vs vs) ++ context) e of
                                   Left _ -> Right (Lambda vs e)
                                   Right e' -> Right (Lambda vs e')

evaluate context (Rose es) = case es of
  (f : xs) -> case evaluate context f of
    Left err -> Left ("Error evaluating " ++ show f ++ "\n" ++ err)
    Right f' -> case f' of
      Lambda vs exp -> do
        xs' <- mapM (evaluate context) xs
        let vslength = length vs 
        let xslength = length xs'
        case compare vslength xslength of
          LT -> do let (xs'', xs''') = splitAt vslength xs'
                   let context' = zipWith (:=) vs xs'' ++ context
                   res <- evaluate context' exp
                   evaluate context' $ Rose (res:xs''')
          EQ -> evaluate (zipWith (:=) vs xs' ++ context) exp
          GT -> do let (vs'', vs''') = splitAt xslength vs
                   res <- evaluate (zipWith (:=) vs'' xs' ++ context) exp
                   pure $ Lambda vs''' res
        evaluate (zipWith (:=) vs xs' ++ context) exp
      _ -> do
        xs' <- mapM (evaluate context) xs
        return (Rose (f' : xs'))

getMain :: [A] -> [A]
getMain = filter (\ (n := _) -> n == "main")

getIncludes :: [A] -> [A]
getIncludes = filter (\ (n := _) -> n == "include")

repeatDefs :: [A] -> [String]
repeatDefs [] = []
repeatDefs ((n := _) : rest) = case filter (\(m := _) -> n == m) rest of
  [] -> repeatDefs rest
  xs -> map (\(n := _) -> n) xs ++ repeatDefs rest

runLisp :: [A] -> Either String S
runLisp context = case repeatDefs context of
  [] -> case getMain context of
      [("main" := main)] -> evaluate context main
      _ -> Left "No unique main function!\n"
  xs -> Left ("Repeat definitions: " ++ show xs ++ "\n")
