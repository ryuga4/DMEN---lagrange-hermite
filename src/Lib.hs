module Lib where


import Data.List
import Debug.Trace
import Control.Monad.ST
import Data.Array.ST
import Data.Maybe
import Control.Monad

popAt :: Int -> [a] -> (a,[a])
popAt 0 (a:b) = (a,b)
popAt n (a:b) =
  let (a',b') = popAt (n-1) b
  in (a',a:b')

lagrange :: [(Double,Double)] -> Double -> Double
lagrange xys x = sum $ map l' [0..length xys - 1]
  where l' n =
          let ((xi,yi),xys') = popAt n xys


          in yi * product [(x-xj)/(xi-xj) | (xj,yj) <- xys']


fact 0 = 1
fact n = n * fact (n-1)

takeD :: Double -> Int -> [(Double,[Double])] -> Double
takeD x n xys = (snd $ fromJust $ find ((==x) . fst) xys) !! n

nodeN :: Double -> [(Double,[Double])] -> Int
nodeN x xys = length $ snd $ fromJust $ find ((==x) . fst) xys

hermite :: [(Double,[Double])] -> Double -> Double
hermite xys input = runST transformer
  where
    fstCol = concat $ map (\(a,bs)->map (const a) bs) xys
    sndCol = concat  $ map (\(a,b:bs) -> map (const b) (b:bs)) xys
    size = sum $ map (length . snd) xys 
    transformer :: ST s Double 
    transformer = do
      m <- (newArray ((0,0),(size-1,size-1)) Nothing :: ST s (STArray s (Int,Int) (Maybe Double)))
      
      forM_ [0..size-1] $ \i -> do
        writeArray m (0,i) $ Just $ sndCol !! i

      forM_ [(x,y) | x <- [1..size-1], y <- [x..size-1]] $ \(x,y) -> do
        let q1 = fstCol !! y
        let q2 = fstCol !! (y-x)
        
        if q1 /= q2
          then
          do
            Just p1 <- readArray m (x-1,y)
            Just p2 <- readArray m (x-1,y-1)
            writeArray m (x,y) $ Just $ (p1-p2)/(q1-q2)
          else
          do
            let d = takeD q1 x xys
            writeArray m (x,y) $ Just $ d / (fromIntegral $ fact x)

      topOnes <- forM [0..size-1] $ \x -> do
        Just c <- readArray m (x,x)
        return (c, take x fstCol)

      return $ sum $ map (\(a,b) -> a * product (map (\i -> input - i) b)) topOnes



