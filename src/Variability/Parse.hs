module Variability.Parse where

import Control.Monad
import qualified Data.Functor.Identity
import System.IO
import qualified Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Variability.VarLib

languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter <|> char '_',
      Token.identLetter = alphaNum <|> char '_',
      Token.reservedNames = ["tt", "ff", "True", "False", "def", "definedEx"],
      Token.reservedOpNames = ["/\\", "&", "&&", "\\/", "|", "||", "!"]
    }

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

identifier :: Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity String
reserved :: String -> Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity ()
identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp :: String -> Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity ()
reservedOp = Token.reservedOp lexer

parens :: Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity a -> Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity a
parens = Token.parens lexer

integer :: Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity Integer
integer = Token.integer lexer

whiteSpace :: Text.Parsec.Prim.ParsecT String u Data.Functor.Identity.Identity ()
whiteSpace = Token.whiteSpace lexer

bOperators :: [[Operator Char st PresenceCondition]]
bOperators =
  [ [Prefix (reservedOp "!" >> return notPC)],
    [Infix (reservedOp "/\\" >> return (/\)) AssocLeft],
    [Infix (reservedOp "\\/" >> return (\/)) AssocLeft],
    [Infix (reservedOp "&&" >> return (/\)) AssocLeft],
    [Infix (reservedOp "||" >> return (\/)) AssocLeft],
    [Infix (reservedOp "&" >> return (/\)) AssocLeft],
    [Infix (reservedOp "|" >> return (\/)) AssocLeft]
  ]

bTerm :: Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity PresenceCondition
bTerm =
  parens pcExpr
    <|> (reserved "tt" >> return truePC)
    <|> (reserved "truePC" >> return truePC)
    <|> (reserved "True" >> return truePC)
    <|> (reserved "ff" >> return falsePC)
    <|> (reserved "falsePC" >> return falsePC)
    <|> (reserved "False" >> return falsePC)
    <|> (reserved "definedEx" >> parens (fmap mkPCVar identifier))
    <|> (reserved "def" >> parens (fmap mkPCVar identifier))
    <|> (integer >>= \i -> if i == 0 then return falsePC else return truePC)
    <|> fmap mkPCVar identifier

pcExpr :: Parser PresenceCondition
pcExpr = buildExpressionParser bOperators bTerm

parsePC :: String -> PresenceCondition
parsePC str =
  case parse pcExpr "" str of
    Left e -> error $ show e
    Right r -> r
