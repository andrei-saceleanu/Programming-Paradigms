import Data.List
import Data.Char
import Data.Bits


{-47-}
s1::Integer->Bool
s1 1=True
s1 2=True
s1 _=False


mem::(Integer->Bool)->Integer->Bool
mem f x =f x

{-48-}
spow2::Integer->Bool
spow2 x = (((.&.) x (x-1))==0)&&(x/=0)

{-49-}
snat::Integer->Bool
snat x = x>=0

{-50-}
intersection::(Integer->Bool)->(Integer->Bool)->(Integer->Bool)
intersection f1 f2 = \x -> (f1 x)&&(f2 x)

{-51-}
intersection_v2::(Integer->Bool)->(Integer->Bool)->Integer->Bool
intersection_v2 f1 f2 x = (f1 x)&&(f2 x)

{-52-}
toSet::[Integer]->(Integer->Bool)
toSet l = \x -> if elem x l then True else False

{-53-}
capList::[Integer->Bool]->(Integer->Bool)
capList [] = \x -> True
capList (x:xs) = intersection x res where res = capList xs

{-54-}
myfilter::(Integer->Bool)->[Integer]->[Integer]
myfilter f [] = []
myfilter f (x:xs) = if f x then x:res else res where res = myfilter f xs

{-56-}
mymap::(a->b)->[a]->[b]
mymap f []=[]
mymap f (x:xs) = (f x):res where res = mymap f xs

{-60-}
app::[[Integer]]->[Integer]
app l = aux_app l [] where
	aux_app lis acc
		|null lis = acc
		|otherwise = aux_app (tail lis) (acc++(head lis))

{-63-}
myfoldl::(b->a->b)->b->[a]->b
myfoldl _ acc [] = acc
myfoldl f acc l = myfoldl f (f acc (head l)) (tail l)


{-67-}
{-foldl intersection (\x ->True) list //list de ex [snat,spow2]-}

