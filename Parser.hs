module Lisp.Parser (
  S(..),
  A(..),
  sfile,
  s,
  depth,
  symbols,
  module Text.Parsec
) where

import Control.Monad.Identity
import Text.Parsec
import Data.Char
import Text.Parsec.Char

data S = Symbol String
       | Number Integer
       | Quote S
       | Rose [S]
       | F
       | T
       | Lambda [String] S

data A = String := S

symbols :: S -> [String]
symbols (Symbol s) = pure s
symbols (Number x) = []
symbols (Quote s) = symbols s
symbols (Rose ss) = ss >>= symbols
symbols F = []
symbols T = []
symbols (Lambda _ _) = []

depth :: S -> Integer
depth (Symbol _) = 0
depth (Number _) = 0
depth (Quote s) = depth s
depth (Rose xs) = 1 + (maximum $ map depth xs)
depth F = 0
depth T = 0
depth (Lambda _ e) = depth e

encode :: Integer -> S
encode n = Lambda ["f"] $ Lambda ["x"] $ encode' n where
  encode' 0 = Symbol "x"
  encode' n = Rose [Symbol "f", encode' (n - 1)]

identifier :: ParsecT String () Identity String
identifier = do
  x <- noneOf "=\\().#\n\t. '"
  xs <- many (noneOf "=\\().#\n\t '")
  return (x : xs)

number :: ParsecT String () Identity String
number = do
  x <- digit
  xs <- many digit
  return (x : xs)

whitespace :: ParsecT String () Identity String
whitespace = many (oneOf " \n\t")

symbol :: ParsecT String () Identity S
symbol = do
  i <- identifier
  pure $ Symbol i

snumber :: ParsecT String () Identity S
snumber = do
  nstr <- number
  let digs  = map (toInteger . digitToInt) nstr
  let digs' = reverse digs
  let digs'' = zipWith (*) [10 ^ n | n <- [0..]] digs'
  return (encode $ sum digs'')

slist :: ParsecT String () Identity S
slist = do
  whitespace *> char '('
  slist <- many (whitespace *> s <* whitespace)
  whitespace *> char ')'
  return (Rose slist)

slambda :: ParsecT String () Identity S
slambda = do
  whitespace *> char '\\'
  var <- many1 $ whitespace *> identifier <* whitespace
  whitespace *> string "." <* whitespace
  exp <- s
  return (Lambda var exp)

squote :: ParsecT String () Identity S
squote = do
  char '\''
  exp <- s
  return (Quote exp)

sbool :: ParsecT String () Identity S
sbool = char '#' *> ((char 't' *> pure T) <|> (char 'f' *> pure F))

s :: ParsecT String () Identity S
s = sbool <|> snumber <|> symbol <|> squote <|> slambda <|> slist

a :: ParsecT String () Identity A
a = do
  is <- many1 (whitespace *> identifier <* whitespace)
  whitespace *> char '='
  case is of
    [i] -> do
      s <- whitespace *> s
      return (i := s) 
    (i:xs) -> do
      s <- whitespace *> s
      return (i := Lambda xs s)


sfile = many (whitespace *> a <* whitespace)

instance Show S where
  show (Symbol str) = str
  show (Number num) = show num
  show (Rose s) = case ("(" ++ (do
    a <- s
    ' ' : show a) ++ ")") of
      "()" -> "()"
      '(':' ':xs -> '(':xs
      x -> x
  show F = "#f"
  show T = "#t"
  show (Quote s) = '\'':show s
  show (Lambda vars exp) =
    let vs = vars >>= (' ':) in
        case vs of
          ' ':xs -> '\\':xs ++ ('.' : show exp)
          xs -> '\\':xs ++ ('.' : show exp)

instance Show A where
  show (a := b) =  a ++ ' ' : '=' : ' ' : show b
