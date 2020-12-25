module Main where

evaluateKey :: Int -> Int -> Int 
evaluateKey subjectNumber = evaluateKey' 1
    where
        evaluateKey' value 0 = value
        evaluateKey' value n = evaluateKey' (value * subjectNumber `mod` 20201227) (n-1)

allKeys :: [(Int, Int)]
allKeys = (0, 1) : [ (n+1, k * 7 `mod` 20201227) | (n, k) <- allKeys ] 

findLoopSize :: Int -> Int
findLoopSize key = head [ n | (n, k) <- allKeys, k == key ]

getEncryptionKey :: Int -> Int -> Int
getEncryptionKey cardPublicKey doorPublicKey = evaluateKey doorPublicKey cardLoopSize
    where
        cardLoopSize = findLoopSize cardPublicKey

main :: IO ()
main = do
    cardPublicKey <- read <$> getLine 
    doorPublicKey <- read <$> getLine
    let encryptionKey = getEncryptionKey cardPublicKey doorPublicKey
    print encryptionKey
