{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Language.Prolog.Quoter (pr, tr, term, pred, program, spcs) where
import Text.Parsec hiding ((<|>), many)
import Language.Haskell.TH.Quote
import Language.Prolog.DataTypes
import Control.Applicative hiding (empty)
import Prelude hiding (pred)
import Language.Haskell.TH hiding (Pred)
import Data.Generics

spcs :: Monad m => ParsecT String u m ()
spcs = (string "%" *> manyTill anyToken (lexeme newline) *> return ()) <|> spaces

lexeme :: Monad m => ParsecT String u m a -> ParsecT String u m a 
lexeme p = p <* spcs
symbol str = lexeme $ string str

ident_tail, ident :: Monad m => ParsecT String u m String
ident_tail = lexeme $ many (oneOf "\\;/-=><=~^#@?$&!+*"<|>alphaNum)
ident = ((++) <$> count 1 (oneOf "\\;/-><=~#$^@&!?+*"<|>lower) <*> ident_tail)

variable, value, wild :: Monad m => ParsecT String u m Val
variable = flip Any 0 <$> ((++) <$> count 1 upper <*> ident_tail)
value = Exists <$> ident
wild  = Wild <$ symbol "_"

number :: Monad m => ParsecT String u m Val
number = Exists <$> many1 digit

list :: Monad m => ParsecT String u m Pred
list = between (symbol "[") (symbol "]") (
      try ( do
        elms <- preds
        symbol "|"
        tls <- pred
        return $ foldr (\a b -> App "." [a,b]) tls elms)
  <|> foldr (\a b->App "." [a,b]) (Val $ Exists "[]") <$> preds
 )

preds :: Monad m => ParsecT String u m [Pred]
preds = lexeme (pred `sepBy` (symbol "," ))

pred :: Monad m => ParsecT String u m Pred
pred = try (App <$> ident <* (symbol "(") <*> preds <* (symbol ")"))
   <|> Val <$> (variable <|> value <|> number <|> wild)
   <|> list

term :: Monad m => ParsecT String u m Term
term = try( flip (:-) [] <$> pred <* symbol ".")
   <|> (:-) <$> pred <* symbol ":-" <*> sepBy pred (symbol ",") <* symbol "."
 <?> "term"

program :: Monad m => ParsecT String u m [Term]
program = spcs *> (term `sepBy` spcs) <* eof

pr = QuasiQuoter (parserToExpQ pred) (parserToPatQ pred)
tr = QuasiQuoter (parserToExpQ term) (parserToPatQ term)

parserToExpQ p src = do
  ans <- runPT (spcs *> p <* eof) () "quote" src
  trm <- either (fail . show) return ans
  dataToExpQ (const Nothing) trm

parserToPatQ p src = do
  ans <- runPT (spcs *> p <* eof) () "quote" src
  trm <- either (fail . show) return ans
  dataToPatQ (const Nothing) trm