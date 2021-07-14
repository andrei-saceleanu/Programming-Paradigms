import Data.Array

naive :: String -> String -> Int
naive a b = d (length a) (length b)
    where
        d i 0 = i
        d 0 j = j
        d i j
            |a!!(i-1) == b!!(j-1) = d (i-1) (j-1)
            |otherwise = minimum [ d (i - 1) j       + 1
                                , d i (j - 1)       + 1
                                , d (i - 1) (j - 1) + 1
                                ]

basic :: String -> String -> Int
basic a b = d m n
    where
        (m,n) = (length a,length b)
        bounds = ((0,0),(m,n))
        d i 0 = i
        d 0 j = j
        d i j
            |a!!(i-1) == b!!(j-1) = ds ! (i-1,j-1)
            |otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]
        ds = listArray bounds [d i j |(i,j)<-(range bounds)]
