import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Numeric (readHex)
import Control.Monad
import Control.Applicative

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'


p_pair :: CharParser () (String, Maybe String)
-- p_pair = do
--   name <- many1 p_char
--   value <- optionMaybe (char '=' >> many p_char)
--   return (name, value)
-- p_pair =
--     liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))
p_pair =
    liftA2 (,) (many1 p_char) (optionMaybe (char '=' *> many p_char))

p_char :: CharParser () Char
-- p_char = oneOf urlBaseChars
--      <|> (char '+' >> return ' ')
--      <|> p_hex
p_char = oneOf urlBaseChars <|> (' ' <$ char '+')
         <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
-- p_hex = do
--   char '%'
--   a <- hexDigit
--   b <- hexDigit
--   let ((d, _):_) = readHex [a,b]
--   return . toEnum $ d
p_hex = let hexify a b = toEnum . fst . head . readHex $ [a, b]
        in hexify <$> (char '%' *> hexDigit) <*> hexDigit
