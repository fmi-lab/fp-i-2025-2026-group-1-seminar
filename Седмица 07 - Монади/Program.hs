module Main where

getValue :: Read a => IO a
getValue = read <$> getLine

getValues :: Read a => IO [a]
-- getValues = map read . words <$> getLine
getValues = do
  line <- getLine
  let values = words line
  let result = map read values
  return result

printList :: Show a => String -> [a] -> IO ()
printList delim = mapM_ (putStr . (++ delim) . show)

main :: IO ()
main = printList ", " [1..5]