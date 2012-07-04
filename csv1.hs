import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
-- csvFile = do
--   result <- many line
--   eof
--   return result
csvFile = endBy line eol

line :: GenParser Char st [String]
-- line = do
--   result <- cells
--   eol
--   return result
line = sepBy cellContent (char ',')

-- cells :: GenParser Char st [String]
-- cells = do
--   first <- cellContent
--   next <- remainingCells
--   return (first : next)

-- remainingCells :: GenParser Char st [String]
-- remainingCells = 
--     (char ',' >> cells)
--     <|> (return [])

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

eol :: GenParser Char st String
-- eol = char '\n'
-- eol = do
--   char '\n'
--   char '\r' <|> return '\n'
eol = try (string "\r\n") <|> try (string "\n\r") <|>
      string "\r" <|> string "\n"
      <?> "Couldn't find EOL"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
