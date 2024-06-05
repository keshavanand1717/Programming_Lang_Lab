-- Function to generate prime numbers up to a given limit
sieve :: Int -> [Int]
sieve n = sieve' [2..n]
    where
        sieve' [] = []   -- base case
        --Recursively eliminating non-prime numbers
        sieve' (x:xs) = x : sieve' [y | y <- xs, y `mod` x /= 0]

-- Main function to take input from the user and print prime numbers
main :: IO ()
main = do
    putStrLn "Please the limit:"
    limit <- getLine        -- reading input
    let n = read limit :: Int
    putStrLn $ "Prime numbers up to " ++ limit ++ " are: " ++ show (sieve n)
