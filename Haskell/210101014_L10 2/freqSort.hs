import Data.List (sort, group, sortBy)
import Data.Function (on)

-- Function to count the frequency of each character in a string
frequency :: String -> [(Char, Int)]
frequency str = map (\x -> (head x, length x)) (group (sort str))

-- Function to sort characters based on their frequency and ASCII values
sortCharacters :: String -> String
sortCharacters str = concatMap (\(c, f) -> replicate f c) sortedFreq
  where
    freq = frequency str
    sortedFreq = sortByFreqAndASCII freq

-- Sort by frequency first, then by ASCII value
sortByFreqAndASCII :: [(Char, Int)] -> [(Char, Int)]
sortByFreqAndASCII = sortBy (\(c1, f1) (c2, f2) -> if f1 /= f2 then compare f1 f2 else compare c1 c2)

-- Main function to read input from the user and display the result
main :: IO ()
main = do
    putStrLn "Enter a string:"
    inputString <- getLine
    putStrLn $ "Original String: " ++ inputString
    putStrLn $ "Sorted Characters: " ++ sortCharacters inputString
