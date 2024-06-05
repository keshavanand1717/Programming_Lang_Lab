-- Importing minimumBy function from Data.List module and on function from Data.Function module
import Data.List (minimumBy)
import Data.Function (on)

-- Defining type synonyms for Maze and Coord
type Maze = [[Int]]
type Coord = (Int, Int)

-- Function to check if a given coordinate is valid and not blocked
isValidCoord :: Maze -> Coord -> Bool
isValidCoord maze (x, y) = x >= 0 && x < length maze && y >= 0 && y < length (head maze) && maze !! x !! y == 0

-- Function to find the shortest path from start to end using a modified BFS algorithm
shortestPath :: Maze -> Coord -> Coord -> Maybe [Coord]
shortestPath maze start end
    | start == end = Just [end]
    | otherwise = bfs [(start, [])] []
    where
        bfs [] _ = Nothing
        bfs ((curr, path):queue) visited
            | curr == end = Just (reverse (curr:path))
            | otherwise = bfs (queue ++ nextNodes) (curr:visited)
            where
                nextNodes = [(nextCoord, curr:path) | nextCoord <- filter (isValidCoord maze) (neighbors curr), nextCoord `notElem` visited]

        neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

-- Function to calculate the length of a path
calculatePathLength :: [Coord] -> Int
calculatePathLength = length

-- Function to find the shortest path length and path itself
findShortestPath :: Maze -> (Int, [Coord])
findShortestPath maze = case shortestPath maze (0, 0) (length maze - 1, length (head maze) - 1) of
                            Just path -> (calculatePathLength path, path)
                            Nothing -> (-1, [])

-- Function to read a maze from standard input
readMaze :: IO Maze
readMaze = do
    putStrLn "Enter the number of rows:"
    rows <- readLn
    putStrLn "Enter the number of columns:"
    cols <- readLn
    putStrLn "Enter the maze (0 for empty, 1 for blocked):"
    maze <- sequence [do putStrLn $ "Row " ++ show (i+1) ++ ":"
                         row <- sequence [readLn :: IO Int | _ <- [1..cols]]
                         return row
                     | i <- [0..rows-1]]
    return maze

-- Main function
main :: IO ()
main = do
    putStrLn "Enter the dimensions of the maze (number of rows and columns):"
    putStrLn "Example: 3 3"
    dimensions <- getLine
    let [rows, cols] = map read $ words dimensions :: [Int]
    putStrLn "Enter the maze entries row by row (0 for empty, 1 for blocked):"
    maze <- getMaze rows cols
    let (length, path) = findShortestPath maze
    putStrLn $ "Shortest Path Length: " ++ show length
    putStrLn $ "Shortest Path: " ++ show path

-- Function to get the maze entries from user input
getMaze :: Int -> Int -> IO Maze
getMaze rows cols = sequence [getRow cols rowNum | rowNum <- [1..rows]]

-- Function to get a single row of maze entries from user input
getRow :: Int -> Int -> IO [Int]
getRow cols rowNum = do
    putStrLn $ "Enter row " ++ show rowNum ++ ":"
    line <- getLine
    let row = map read $ words line :: [Int]
    if length row /= cols
        then do
            putStrLn $ "Row length should be " ++ show cols ++ ". Please enter again."
            getRow cols rowNum
        else return row
