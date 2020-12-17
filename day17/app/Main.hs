{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet) 
import Data.Hashable ( Hashable(..) )

newtype Cube3D = Cube3D (Int, Int, Int) deriving (Eq, Hashable)
newtype Cube4D = Cube4D (Int, Int, Int, Int) deriving (Eq, Hashable)
type State c = HashSet c

to4D :: Cube3D -> Cube4D
to4D (Cube3D (x, y, z)) = Cube4D (x, y, z, 0)

parseState :: String -> State Cube3D
parseState text = HashSet.fromList [Cube3D (x, y, 0) | (y, line) <- zip [0..] (lines text), 
                                                       (x, c) <- zip [0..] line,
                                                       c == '#']

class (Eq c, Hashable c) => Cube c where
    neighbours :: c -> HashSet c

instance Cube Cube3D where
    neighbours (Cube3D (x, y, z)) = HashSet.fromList [
            Cube3D (x+dx, y+dy, z+dz) | dx <- [-1, 0, 1],
                                        dy <- [-1, 0, 1],
                                        dz <- [-1, 0, 1],
                                        dx /= 0 || dy /= 0 || dz /= 0 
        ]

instance Cube Cube4D where
    neighbours (Cube4D (x, y, z, w)) = HashSet.fromList [
            Cube4D (x+dx, y+dy, z+dz, w+dw) | dx <- [-1, 0, 1],
                                              dy <- [-1, 0, 1],
                                              dz <- [-1, 0, 1],
                                              dw <- [-1, 0, 1],
                                              dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0
        ]

staysActive :: Cube c => State c -> c -> Bool
staysActive activeCubes cube = numActiveNeighbours == 2 || numActiveNeighbours == 3
    where
        activeNeighbours = HashSet.intersection activeCubes $ neighbours cube
        numActiveNeighbours = HashSet.size activeNeighbours

becomesActive :: Cube c => State c -> c -> Bool
becomesActive activeCubes cube = numActiveNeighbours == 3
    where
        activeNeighbours = HashSet.intersection activeCubes $ neighbours cube 
        numActiveNeighbours = HashSet.size activeNeighbours

step :: Cube c => State c -> State c
step activeCubes = HashSet.union stillActive newlyActive
    where
        stillActive = HashSet.filter (staysActive activeCubes) activeCubes 
        newlyActive = HashSet.filter (becomesActive activeCubes) inactiveNeighbours
        inactiveNeighbours = HashSet.difference allNeighbours activeCubes 
        allNeighbours = HashSet.unions $ map neighbours $ HashSet.toList activeCubes

run :: Cube c => Int -> State c -> State c
run 0 = id
run n = run (n-1) . step

main :: IO ()
main = do
    initialState <- fmap parseState getContents 
    let endState3d = run 6 initialState
    putStrLn $ show (HashSet.size endState3d) ++ " cubes active (3D)"
    let initialState4d = HashSet.map to4D initialState
    let endState4d = run 6 initialState4d
    putStrLn $ show (HashSet.size endState4d) ++ " cubes active (4D)"

