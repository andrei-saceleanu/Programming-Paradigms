
--8.1.1
nat :: [Integer]
nat = 0:(map (+1) nat)

--8.1.2
odds :: [Integer]
odds = zipWith (+) nat (tail nat)

--8.1.3
fibo :: [Integer]
fibo = 1:1:(zipWith (+) fibo (tail fibo))

--extra
sieve :: [Integer]
sieve = aux (map (+2) nat)
    where
        aux (x:xs) = x:(aux (filter (\y -> y `mod` x /= 0) xs))

--8.2.1
build :: (a->a) -> a -> [a]
build g a0 = a0:(build g (g a0))

--8.2.2
select :: (Num a,Ord a) => a -> [a] -> a
select e [] = e
select e [x] = x
select e (x:y:xs) = if (abs (x-y))<e then x else select e (y:xs)

--8.2.3
ratio = zipWith (/) (map (\x -> fromIntegral x::Float) (tail fibo)) (map (\x -> fromIntegral x::Float) fibo)

phi = select 0.001 ratio

--8.2.4
an = build (\x -> x + (sin x)) 0.5

my_pi = select 0.001 an

--8.2.5
k = 100
bn = build (\x -> 0.5 * (x + k/x)) 0.5

sqrtk = select 0.001 bn

--8.2.6
newton :: (Float->Float) -> (Float -> Float) -> Float
newton f fprim = select 0.001 (build (\x -> x - (f x)/(fprim x)) 0.5)

--8.2.7
derivative :: (Float -> Float) -> Float -> Float
derivative f a = select 0.001 (map (\h-> ((f (a+h)) - (f a))/h ) sequ)
    where
        h0 = 0.5
        sequ = build (/2) h0

--8.2.8
--a
trapez2 :: (Float->Float) -> Float -> Float -> Float
trapez2 f a b = (b-a)*((f a)+(f b))/2

--b
insert_middle :: [Float] -> [Float]
insert_middle [] = []
insert_middle [x] = [x]
insert_middle (x:y:xs) = x:((x+y)/2):(insert_middle (y:xs))

--c
areas :: (Float->Float) -> [Float] -> [Float]
areas _ [] = []
areas _ [x] = []
areas f (x:y:xs) = (trapez2 f x y):(areas f (y:xs))

--d
integral :: (Float->Float) -> Float -> Float -> Float
integral f a b  = sum (areas f (head (drop 10 (build insert_middle [a,b]))))


pow_x::Integer->[Integer]
pow_x x = build (*x) 1

fact::Integer-> Integer
fact 0 = 1
fact n = n * (fact (n-1))

factorials = map fact nat

terms x = zipWith (\x y -> (fromIntegral x::Float)/(fromIntegral y::Float)) (pow_x x) factorials
aprox x = foldr (+) 0 (terms x)


















