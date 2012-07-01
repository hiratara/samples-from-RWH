module Prettify2 where

data Doc = Empty | Char Char | Text String | Line
                 | Concat Doc Doc | Union Doc Doc
                   deriving (Show, Eq)

empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
Empty <> d2 = d2
d1 <> Empty = d1
d1 <> d2 = Concat d1 d2

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line
