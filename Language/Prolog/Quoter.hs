{-# LANGUAGE RankNTypes #-}
module Language.Prolog.Quoter (tr, pr) where
import Text.Parsec hiding ((<|>), many)
import Language.Haskell.TH.Quote
import Language.Haskell.TH hiding (Pred)
import Language.Prolog.DataTypes
import Control.Applicative hiding (empty)
import Prelude hiding (pred)

lexeme :: Monad m => ParsecT String u m a -> ParsecT String u m a 
lexeme p = p <* spaces
symbol str = lexeme $ string str

ident_tail = lexeme $ many alphaNum
ident = ((++) <$> count 1 lower <*> ident_tail)

variable = Any <$> ((++) <$> count 1 upper <*> ident_tail)
value = Exists <$> ident

pred = try (App <$> ident <*> between (symbol "(") (symbol ")") (pred `sepBy` symbol ","))
   <|> Val <$> (variable <|> value)

term = (try ( (:-) <$> pred <* symbol ":-" <*> sepBy pred (symbol ",") )
        <|> flip (:-) [] <$> pred) <* symbol "."

program = sepBy term newline

pr = QuasiQuoter (parserToQ pred $ dataToExpQ (const Nothing)) (parserToQ pred $ dataToPatQ (const Nothing))
tr = QuasiQuoter (parserToQ term $ dataToExpQ (const Nothing)) (parserToQ term $ dataToPatQ (const Nothing))

parserToQ p dataToQ src = do
  ans <- runPT (spaces *> p <* eof) () "quote" src
  trm <- either (fail . show) return ans
  dataToQ trm
  

