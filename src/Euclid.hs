module Euclid
where

import Graphics.GD
import Data.Bits
import Data.List
import System.Random

type Position = (Int, Int)

dim = 100
d = dim `div` 2
randomPoints = 2 * dim

data CellType = Empty | Wall | Start | Finish | Npc deriving (Show, Read, Eq)

generateMap :: [Position] -> [Position] -> [(Position, CellType)]
generateMap grid walls = map (\x -> (x, getVal' x)) grid
  where getVal' (x, y) = getVal (x - d, y - d)
        getVal (m,n) | (m,n) `elem` walls = Wall
                     | m == 0 || n == 0   = Empty
                     | gcd m n == 1       = Empty
                     | otherwise          = Wall

makeLazyMap pts = concat $ map (\x -> getTile (snd x) ++ isEnd (fst x)) pts
  where isEnd (x,y) | y == dim  = "\n"
                    | otherwise = ""
        getTile x | x == Empty  = "00 "
                  | x == Wall   = "03 "
                  | x == Start  = "00 "
                  | x == Finish = "01 "
                  | x == Npc    = "04 "

drawPoints img scale ((x,y), val) = drawFilledRectangle (x',y') (x' + scale,y' + scale) (getColor) img
  where (x',y') = ((x)*scale, (y)*scale)
        getColor | val == Empty  = rgbToCInt (255,255,255)
                 | val == Wall   = rgbToCInt (0,0,0)
                 | val == Start  = rgbToCInt (0,255,0)
                 | val == Finish = rgbToCInt (255,0,0)
                 | val == Npc    = rgbToCInt (0,0,255)
        rgbToCInt (r,g,b) = toEnum $ rotateL r 16 + rotateL g 8 + b

makeRandomPoints :: IO [Position]
makeRandomPoints = do gen  <- newStdGen 
		      let list = randomRs (1, dim - 1) gen
                      return $ makeList list []
   where makeItem xs = (xs !! 0, xs !! 1) 
         makeList xs ys | length ys == randomPoints = ys
                        | otherwise = makeList (tail (tail xs)) (checkList xs ys)
         checkList xs ys | (makeItem xs) `elem` ys = ys
                         | otherwise               = ys ++ [makeItem xs]

makeMaze :: [Position] -> IO ([(Position, CellType)], Position, [Position], [Position])
makeMaze b  = do walls <- makeRandomPoints 
                 return (maze walls, (0,0), take 3 (drop 1 walls), take 5 (drop 3 walls))
  where maze walls = generateMap b (drop 8 walls)
  
boardImage :: [Position] -> Int -> String -> IO()
boardImage b scale filename = do
  img <- newImage (dim*scale, dim*scale)
  mazePts <- makeMaze b 
  mapM_ (drawPoints img scale) $ pntArray mazePts 
  savePngFile (filename ++ ".png") img
  writeFile (filename ++ ".map") $ makeLazyMap (pntArray mazePts)
  where pntArray pts = map last $ groupBy (\x y -> fst x == fst y) ( pntArray' pts ) 
        pntArray' pts = sortBy (\x y -> compare (fst x) (fst y)) $ pntArray'' pts
        pntArray'' (maze, start, fin, npcs) = concat [ maze
                                                     , [(start, Start)] 
                                                     , map (\x -> (x, Finish)) fin 
                                                     , map (\x -> (x, Npc)) npcs
                                                     ]

