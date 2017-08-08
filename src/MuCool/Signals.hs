module MuCool.Signals where

crossCorrelation :: [Double] -> [Double] -> [Double]
crossCorrelation x y = [h j | j <- [(-(n-1))..(m-1)]]
    where n = length x
          m = length y
          (!!!) :: [Double] -> Int -> Double
          (!!!) list index
            | (index < 0) || (index >= len) = 0.0
            | otherwise = list !! index
            where len = length list
          h :: Int -> Double
          h j = sum [(x !!! k) * (y !!! (j+k)) | k <- [0..(n-1)]]
          {-
          cross_correlation_elem :: [Double] -> [Double] -> Int -> Double
          cross_correlation_elem j
            | j < 0 = sum (zipWith (*) x (zeros ++ (take (n+j) y)))
            | j > 0 = sum (zipWith (*) x ((drop j y) ++ zeros))
            | j == 0 = sum (zipWith (*) x y)
            | otherwise = sum (zipWith (*) x y)
              where n = length x
                    m = length y
                    zeros = replicate (abs j) 0.0
          -}
