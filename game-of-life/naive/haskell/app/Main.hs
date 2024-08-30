module Main (
    main
) where

data Cell = Live | Dead deriving (Eq, Ord)
data Position = Position { xx :: Int, yy :: Int } deriving (Eq, Ord, Show)

instance Show Cell where
    show Live = "*"
    show Dead = "."

-- Define the size of the grid
width, height :: Int
width = 10
height = 10

-- Initialize the grid with a default state
initializeGrid :: (Position -> Cell) -> [[Cell]]
initializeGrid getState =
    [[ getState (Position x y) | x <- [0..width-1]] | y <- [0..height-1]]

-- Seed function that returns Live for positions where x + y is even, otherwise Dead
seedStateFn :: Position -> Cell
seedStateFn (Position x y) = if even (x + y) then Live else Dead

-- Collect the neighbors of a given cell
collectNeighbors :: Position -> [Position]
collectNeighbors (Position x y) =
    [ Position (x + dx) (y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]      -- all 8 neighbors
    , (dx, dy) /= (0, 0)    -- skip the cell itself
    , x + dx >= 0, x + dx < width   -- ensure grid boundaries
    , y + dy >= 0, y + dy < height
    ]

-- Count live neighbors of a given cell
countLiveNeighbors :: [[Cell]] -> Position -> Int
countLiveNeighbors grid pos =
    length $ filter (== Live) $ map getCell (collectNeighbors pos)
  where
    getCell (Position nx ny) = grid !! ny !! nx

-- Determine the next state of a cell
nextState :: Cell -> Int -> Cell
nextState Live liveNeighbors
    | liveNeighbors < 2 = Dead
    | liveNeighbors == 2 || liveNeighbors == 3 = Live
    | liveNeighbors > 3 = Dead
nextState Dead liveNeighbors
    | liveNeighbors == 3 = Live
    | otherwise = Dead
nextState _ _ = error "Invalid state"   -- how come we don't have exhaustive pattern match above?

-- Generate the next grid based
nextGeneration :: [[Cell]] -> [[Cell]]
nextGeneration grid =
    [ [ nextState (grid !! y !! x) (countLiveNeighbors grid (Position x y))
      | x <- [0..width-1] ] -- for each cell in the row
    | y <- [0..height-1] ] -- for each row

-- Create a grid
buildGrid :: [[Cell]]
buildGrid = initializeGrid seedStateFn

-- Print the grid for visualization
printGrid :: [[Cell]] -> IO ()
printGrid = mapM_ (putStrLn . concatMap show)

-- Generate and print N generations
goLife :: Int -> Int -> [[Cell]] -> IO ()
goLife _ 0 _ = return ()
goLife maxGen n grid = do
    let nextGrid = nextGeneration grid
    putStrLn $ "\nGeneration " ++ show (maxGen - n + 1) ++ ":"
    printGrid nextGrid
    goLife maxGen (n - 1) nextGrid


main :: IO ()
main = do
    let grid' = buildGrid
    putStrLn "\nSeed Grid:"
    printGrid grid'
    let numGenerations = 6
    goLife numGenerations numGenerations grid'
