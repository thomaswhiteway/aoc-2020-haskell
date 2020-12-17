module Main where
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)

type Cube = (Int, Int, Int)
type State = HashSet Cube 

parseState :: String -> State
parseState text = HashSet.fromList [(x, y, 0) | (y, line) <- zip [0..] (lines text), 
                                                (x, c) <- zip [0..] line,
                                                c == '#']

neighbours :: Cube -> HashSet Cube
neighbours (x, y, z) = HashSet.fromList [(x+dx, y+dy, z+dz) | dx <- [-1, 0, 1],
                                                              dy <- [-1, 0, 1],
                                                              dz <- [-1, 0, 1],
                                                              dx /= 0 || dy /= 0 || dz /= 0 ]

staysActive :: State -> Cube -> Bool
staysActive activeCubes cube = numActiveNeighbours == 2 || numActiveNeighbours == 3
    where
        activeNeighbours = HashSet.intersection activeCubes $ neighbours cube
        numActiveNeighbours = HashSet.size activeNeighbours

becomesActive :: State -> Cube -> Bool
becomesActive activeCubes cube = numActiveNeighbours == 3
    where
        activeNeighbours = HashSet.intersection activeCubes $ neighbours cube 
        numActiveNeighbours = HashSet.size activeNeighbours

step :: State -> State
step activeCubes = HashSet.union stillActive newlyActive
    where
        stillActive = HashSet.filter (staysActive activeCubes) activeCubes 
        newlyActive = HashSet.filter (becomesActive activeCubes) inactiveNeighbours
        inactiveNeighbours = HashSet.difference allNeighbours activeCubes 
        allNeighbours = HashSet.unions $ map neighbours $ HashSet.toList activeCubes

run :: Int -> State -> State
run 0 = id
run n = run (n-1) . step

main :: IO ()
main = do
    initialState <- fmap parseState getContents 
    let endState = run 6 initialState
    putStrLn $ show (HashSet.size endState) ++ " cubes active"

