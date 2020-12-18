{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet) 
import Data.Hashable ( Hashable(..) )

newtype Cube3D = Cube3D (Int, Int, Int) deriving (Eq, Hashable)
newtype Cube4D = Cube4D (Int, Int, Int, Int) deriving (Eq, Hashable)
type State c = HashSet c

printGrid :: State Cube3D -> String
printGrid state = unlines $ concat [["z=" ++ show z] ++ [[if Cube3D (x,y,z) `HashSet.member` state then '#' else '.' | x <- [minimum xs..maximum xs]] | y <- [minimum ys..maximum ys]] ++ [""] | z <- [minimum zs..maximum zs]]
    where
        xs = [x | Cube3D (x, _, _) <- HashSet.toList state]
        ys = [y | Cube3D (_, y, _) <- HashSet.toList state]
        zs = [z | Cube3D (_, _, z) <- HashSet.toList state]


to4D :: Cube3D -> Cube4D
to4D (Cube3D (x, y, z)) = Cube4D (x, y, z, 0)

parseState :: String -> State Cube3D
parseState text = HashSet.fromList [Cube3D (x, y, 0) | (y, line) <- zip [0..] (lines text), 
                                                       (x, c) <- zip [0..] line,
                                                       c == '#']

class Cube c where
    neighbours :: c -> [c]

instance Cube Cube3D where
    neighbours (Cube3D (x, y, z)) =  [
            Cube3D (x+dx, y+dy, z+dz) | dx <- [-1, 0, 1],
                                        dy <- [-1, 0, 1],
                                        dz <- [-1, 0, 1],
                                        dx /= 0 || dy /= 0 || dz /= 0 
        ]

instance Cube Cube4D where
    neighbours (Cube4D (x, y, z, w)) = [
            Cube4D (x+dx, y+dy, z+dz, w+dw) | dx <- [-1, 0, 1],
                                              dy <- [-1, 0, 1],
                                              dz <- [-1, 0, 1],
                                              dw <- [-1, 0, 1],
                                              dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0
        ]

neighboursWithout :: (Cube c, Hashable  c, Eq c) => State c -> c -> [c]
neighboursWithout state c = filter (not . flip HashSet.member state) $ neighbours c

addNeighbours ::  (Cube c, Hashable  c, Eq c) => c -> State c -> State c
addNeighbours cube state = foldr HashSet.insert state $ neighboursWithout state cube

staysActive :: (Cube c, Hashable  c, Eq c) => State c -> c -> Bool
staysActive activeCubes cube = numActiveNeighbours == 2 || numActiveNeighbours == 3
    where
        activeNeighbours = filter (`HashSet.member` activeCubes) $ neighbours cube
        numActiveNeighbours = length activeNeighbours

becomesActive :: (Cube c, Hashable  c, Eq c) => State c -> c -> Bool
becomesActive activeCubes cube = numActiveNeighbours == 3
    where
        activeNeighbours = filter (`HashSet.member` activeCubes) $ neighbours cube
        numActiveNeighbours = length activeNeighbours

step :: (Cube c, Hashable  c, Eq c) => State c -> State c
step activeCubes = HashSet.union stillActive newlyActive
    where
        stillActive = HashSet.filter (staysActive activeCubes) activeCubes 
        newlyActive = HashSet.filter (becomesActive activeCubes) inactiveNeighbours
        inactiveNeighbours = HashSet.difference allNeighbours activeCubes 
        allNeighbours = HashSet.foldr addNeighbours HashSet.empty activeCubes

run :: (Cube c, Hashable  c, Eq c) => Int -> State c -> State c
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

