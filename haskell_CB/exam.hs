nats = 0:(map (+1) nats)

f::[Integer]->[Integer]
f l = aux l 0 []
    where
        aux l s acc
            |null l = acc++[s]
            |otherwise = aux (tail l) (s+(head l)) (acc++[s])

build :: (a->a) -> a -> [a]
build g a0 = a0:(build g (g a0))

pow_x x = build (*x) 1

fact 0 = 1
fact n = n * (fact (n-1))

facts = map fact nats

exp_x x = zipWith (\x y -> (fromIntegral x::Float)/(fromIntegral y::Float)) (pow_x x) facts

aprox l = aux l 0
    where
        aux l n = (sum (take n l)):(aux l (n+1)) 