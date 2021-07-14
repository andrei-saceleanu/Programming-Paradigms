import Data.List
import Data.Char

{-1-}

f1::Int->Int
f1 x=1

f1p::Int->Int
f1p = \x -> 1

{-2-}

f2::Int->Int->Int
f2 x y=x

{-3-}

myand::Bool->Bool->Bool

myand x y
	|x==True && y==True=True
	|otherwise = False

{-4-}
-- f1::Int->Int
-- f2::Int->Int->Int

{-5-}

daca::Bool->a->a->a
daca x y z=if x then y else z

{-6-}
f::Integer->Integer->Integer->Integer
f x y z=max x (max y z)


{-7-}
-- f::Bool->a->a->a
{-8-}
-- f::Bool->a->a->a

{-9-}
fa::Integer->Integer->Integer->Integer
fa x y z
	|x>y && x>z=x
	|y>z && y>x=y
	|z>x && z>y=z

{-10-}
-- da
-- nu

{-11-}
fb::Integer->Integer->Integer->Integer
fb x y z=last (sort [x,y,z])


{-12-}
myrev::[a]->[a]
myrev [] = []
myrev (x:xs) = (myrev xs)++[x]

{-13-}
fc::Integral a=>[a]->Bool
fc l
	|length l<3 = False
	|otherwise = (mod (l!!i) 2)==0 
	where i=(length l)-2

{-14-}
mysum::Num a=>[a]->a
mysum []=0
mysum (x:xs) = x+(mysum xs)

{-15-}
bo::[Bool]->Bool
bo []=True
bo (x:xs) = x&&(bo xs)

{-16-}
myfilter::Integral a=>[a]->[a]
myfilter []=[]
myfilter (x:xs) = if (mod x 2)==0 then x:res else res where res=myfilter xs

{-17-}
booltoint::[Bool]->[Int]
booltoint []=[]
booltoint (x:xs) = if x==True then 1:res else 0:res where res=booltoint xs

{-18-}
fd :: [[Integer]] -> [Bool]
fd [] = []
fd l = (g (head l)):(fd (tail l))
	where
	g [] = True
	g l = h (tail l)
	h [] = True
	h l = False

-- intoarce lista de booleans (True daca lungimea listei curente
-- din lista de liste e maxim 1 si False altfel) 


{-19-}
sumbool::[Bool]->Int
sumbool [] = 0
sumbool (x:xs) = if x==True then 1+res else res where res = sumbool xs

{-20-}
insert_sort::Ord a=>a->[a]->[a]
insert_sort x [] = [x]
insert_sort x (y:ys) = if x<y then x:y:ys else y:(insert_sort x ys)

inssort::Ord a=>[a]->[a]
inssort [] = []
inssort (x:xs) =insert_sort x (inssort xs) 


