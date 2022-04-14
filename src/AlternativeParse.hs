module AlternativeParse where

import Control.Monad
import Data.Functor
import Expr
import Text.Parsec
import Text.Parsec.String

type ParserExpr = Parsec String ()

parseExpr :: Parser (Expr String String)
parseExpr =
  try parseTypeApp
    <|> try parseApp
    <|> parseTerm

parseApp :: Parser (Expr String String)
parseApp = chainl1 parseTerm (return Application)

parseTypeApp :: Parser (Expr String String)
parseTypeApp =
  TypeApplication
    <$> parseTerm
    <*> typ'
  where
    typ' = parseChar '[' *> typ <* parseChar ']'

parseTerm :: Parser (Expr String String)
parseTerm = try parseAbs <|> parseTypeAbs <|> parseVar <|> parens parseExpr

parseVar :: Parser (Expr String String)
parseVar = Var <$> exprId

parseAbs :: Parser (Expr String String)
parseAbs =
  curry
    <$> (parseString "fn" *> many1 args <* parseString "=>")
    <*> parseExpr
  where
    args = (,) <$> (exprId <* parseChar ':') <*> typ
    curry = flip . foldr . uncurry $ Abstraction

parseTypeAbs :: Parser (Expr String String)
parseTypeAbs = curry <$> args <*> parseExpr
  where
    args = parseString "fn" *> many1 typeId <* parseString "=>"
    curry = flip (foldr TypeAbstraction)

typ :: Parser (Type String)
typ = try parseArrow

parseArrow :: Parser (Type String)
parseArrow = chainr1 parseTypeTerm (parseString "->" $> TypeArrow)

parseTypeTerm :: Parser (Type String)
parseTypeTerm = parseTypeVar <|> parens typ

parseTypeVar :: Parser (Type String)
parseTypeVar = TypeVar <$> typeId

parseIdentifier :: Parser Char -> Parser String
parseIdentifier firstChar = lexeme ((:) <$> first <*> many rest)
  where
    first = firstChar <|> char '_'
    rest = first <|> digit

typeId, exprId :: Parser String
typeId = parseIdentifier upper
exprId = parseIdentifier lower

parens :: Parser a -> Parser a
parens p = parseChar '(' *> p <* parseChar ')'

whitespace :: Parser ()
whitespace = void . many . oneOf $ " \t"

parseChar :: Char -> Parser ()
parseChar = try . void . lexeme . char

parseString :: String -> Parser ()
parseString = try . void . lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

altRunParseExpr :: String -> Either ParseError (Expr String String)
altRunParseExpr = parse (whitespace *> parseExpr <* eof) ""

altRunParseType :: String -> Either ParseError (Type String)
altRunParseType = parse (whitespace *> typ <* eof) ""