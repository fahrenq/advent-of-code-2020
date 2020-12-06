-- https://adventofcode.com/2020/day/3

module Main where

enumerate = zip [0 ..]

parseValue :: Char -> Bool
parseValue '#' = True
parseValue '.' = False

toMatrix :: String -> [[Bool]]
toMatrix = map (map parseValue) . lines

getReducer :: Int -> Int -> (Int, Int) -> (Int, [Bool]) -> (Int, Int)
getReducer downStep rightStep (sum, pos) (rowIdx, row) =
  (nextSum, nextPos)
  where
    nextSum = if row !! pos && not skipRow then sum + 1 else sum
    skipRow = downStep > 1 && (rowIdx + 1) `mod` downStep == 0
    width = length row
    posWithRightStep = pos + rightStep
    nextPos =
      if posWithRightStep > width - 1
        then posWithRightStep - width
        else posWithRightStep

ride downStep rightStep matrix =
  foldl (getReducer downStep rightStep) (0, 0) (enumerate matrix)

rideResults input = do
  putStrLn $ "Path D1R1 - " ++ show path11
  putStrLn $ "Path D1R3 - " ++ show path13
  putStrLn $ "Path D1R5 - " ++ show path15
  putStrLn $ "Path D1R7 - " ++ show path17
  putStrLn $ "Path D2R1 - " ++ show path21
  putStrLn $ "Multiplied - " ++ show (path11 * path13 * path15 * path17 * path21)

  where
    matrix = toMatrix input
    path11 = fst $ ride 1 1 matrix
    path13 = fst $ ride 1 3 matrix
    path15 = fst $ ride 1 5 matrix
    path17 = fst $ ride 1 7 matrix
    path21 = fst $ ride 2 1 matrix

main :: IO ()
main = do
  contents <- readFile "input.txt"
  rideResults contents