data Ball = Red | Blue | Yellow deriving Eq

type Board = [[Ball]]


type Pt = (Int, Int)

isValidPos :: Board -> Pt -> Bool
isValidPos [] _ = False
isValidPos (column:_) (0, y) = y >= 0 && y < length column
isValidPos (_:columns) (x, y) = isValidPos columns (x - 1, y)


markBoard :: Board -> Pt -> [Pt]
markBoard board (x, y) = loop (x, y) []
    where ball = board !! x !! y
          loop (x, y) marks
               | isValidPos board (x, y) && board !! x !! y == ball =
                   foldr loop ((x, y):marks) $ filter (flip notElem marks)
                             [(x + 1, y), (x - 1, y),
                              (x, y + 1), (x, y - 1)]
               | otherwise = marks

pickBall :: Board -> Pt -> Board
pickBall board pt = filter ((/= 0) . length) $ zipWith (\x column -> snd $ unzip $ filter (\(y, ball) -> notElem (x, y) marks) (zip [0..] column)) [0..] board
    where marks = markBoard board pt

-- zipWith (\x column -> snd $ unzip $ filter (\y, ball) -> notElem (x, y) marks) $ zip [0..] column) [0..] board
