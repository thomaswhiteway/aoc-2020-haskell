module Main where

type BoardingPass = (Int, Int)

parseSection :: Char -> Int -> Int -> String -> Int
parseSection c = parseSection'
    where
        parseSection' :: Int -> Int -> String -> Int
        parseSection' lower _ [] = lower
        parseSection' lower upper (h:rest) 
            | h == c    = parseSection' lower  middle rest
            | otherwise = parseSection' middle upper  rest 
            where
                middle = (lower + upper) `div` 2

parseBoardingPass :: String -> BoardingPass
parseBoardingPass text = (parseSection 'F' 0 128 rowtext, 
                          parseSection 'L' 0 8 columntext)
    where
        (rowtext, columntext) = splitAt 7 text

getId :: BoardingPass -> Int
getId (row, column) = row * 8 + column

main :: IO ()
main = interact (show . maximum . map (getId . parseBoardingPass) . lines)
