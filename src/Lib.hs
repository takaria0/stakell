module Lib
    (
        -- randomNum,
        randomNumGen,
        minMaxScaling,
        -- gaussDist,
        -- xValueForGauss,
        -- testFunc1,
        -- testFunc2,
    ) where


import Data.List
-- import qualified Data.Vector as V

-- data coordinate = (Double, Double)


-- testFunc1 = minMaxScaling (-1) 1 (randomNum 100)
-- testFunc2 = randomNum 100

-- -- generate Gauss Distribution
-- gaussDist :: Float -> Float -> Integer -> [Float]
-- gaussDist mu sigma size = 
--     y where
--         xs = xValueForGauss mu sigma size
--         a = ( 1 / ( sigma * sqrt (2*pi) ) )
--         y = [ a * exp ( -((x - mu )**2) / (2*(sigma**2)))  | x<- xs]

-- xValueForGauss :: Float -> Float -> Integer -> [Float]
-- xValueForGauss mu sigma size = 
--     xs where
--         xs = minMaxScaling (-(mu+4*sigma)) (mu+4*sigma) (randomNum size) 



-- -- postprocess. from random generator to a list of random integers 
randomNum :: Integer -> [Float]
randomNum size =
    y where
        n = 10
        a = 8121
        b = 28411
        m = 134456
        seed = 1
        seedNum = round (2 * size ) `div` n
        y = [ randomNumGen a b m i seed |i<-[1..n]]
        -- y_num = [[ (randomNumGen a b m i seed) |i<-[1..n]] | seed <- [1..seedNum]]
        -- y = y_num
        -- y = slice 1 size y_num



-- random number generator, linear congression generator
randomNumGen a b m n seed = 
    if n == 1 then mod (a * seed + b) m else mod (a * randomNumGen a b m (n-1) seed + b) m

-- minMaxScaling :: (Float, Float, [a]) -> [(Float, Float, b)]
minMaxScaling :: Float -> Float -> [Float] -> [Float]
minMaxScaling min max values = 
    y where
        y_std = [ (x - minimum(values)) /  (maximum(values) - minimum(values)) | x <- values]
        y = [ y_std_value * (max - min) + min | y_std_value<- y_std]



-- UTILS

-- split function
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- slice
-- slice :: Integer -> Integer -> [a]
slice from to xs = take (to - from + 1) (drop from xs)