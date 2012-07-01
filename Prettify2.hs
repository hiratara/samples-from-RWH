module Prettify2 where

data Doc = Empty | Char Char | Text String | Line
                 | Concat Doc Doc | Union Doc Doc
                   deriving (Show, Eq)

