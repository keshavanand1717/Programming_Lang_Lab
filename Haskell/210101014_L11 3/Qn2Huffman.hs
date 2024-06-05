-- Importing the Data.List module with the alias L
import qualified Data.List as L

-- Function to generate Huffman codes for a given list of character frequencies
huffman :: [(Char, Int)] -> [(Char, [Char])]
huffman x = reformat $ huffman_combine $ resort $ morph x
    where
        -- Morphing the input list to a suitable format for processing
        morph x = [ ([[]],[c],n) | (c,n) <- x ]
        
        -- Sorting the input list based on frequency
        resort x = L.sortBy (\(_,_,a) (_,_,b) -> compare a b) x
        
        -- Function to combine Huffman trees until only one tree remains
        huffman_combine (x:[]) = x
        huffman_combine (x:xs) = huffman_combine $ resort ((combine_elements x (head xs)) : (tail xs))
            where
                -- Combining two elements of the Huffman tree
                combine_elements (a,b,c) (x,y,z) = ( (map ('0':) a) ++ (map ('1':) x), b ++ y, c+z)

        -- Function to reformat the output into (character, Huffman code) pairs
        reformat (x,y,_) = L.sortBy (\(a,b) (x,y) -> compare (length b) (length y)) $ zip y x

-- Main function to take user input and print the result
main :: IO ()
main = do
    putStrLn "Enter a list of tuples (character and its frequency):"
    input <- readLn :: IO [(Char, Int)]
    let result = huffman input
    print result
