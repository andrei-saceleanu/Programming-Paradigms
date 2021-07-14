import Data.List
import Data.Char

{-21-}
{- list!!3 -}

{-22-}
f::[a]->Int
f []=0
f l=if (length l)<4 then 0 else 1

{-24-}
only_positive::(Eq a ,Num a,Ord a,Integral a)=>[a]->[a]
{-only_positive [] = []
only_posiitve (x:xs) = if x>=0 then x:res else res where res=only_positive xs-}
only_positive l = filter (\el -> el>=0) l

{-25-}
{-g :: t -> t1 -> (t, t1)-}

{-27-}
f27::String->[(String,[String])]->[String]
f27 grupa elevi = filter (\nume -> nume!!0=='M') (concatMap snd (filter (\el -> (fst el)==grupa) elevi))

{-28-}
f28::[Int]->Bool
f28 l = let lungime=length l in
		if lungime<3 then False else ((reverse (sort l))!!2)>=0

{-29-}
f29 x = ff x
	where g x = 2*x
	      h x = x + 1
	      ff = h.f29

{-31-}
mysplit::Char->[Char]->[[Char]]
mysplit sep l = auxsplit sep l [] [] where
		auxsplit sep l acc_string acc_list
			|null l = acc_list ++ [acc_string]
			|head l == sep = auxsplit sep (tail l) [] (acc_list ++ [acc_string])
			|otherwise = auxsplit sep (tail l) (acc_string++[head l]) acc_list


students::[(String,Int)]->[([String],Int)]
students [] = []
students l = map  (\x -> if length (fst x) >= 3 then (fst x,(snd x)+1) else x)  (map  (\el -> (mysplit ' ' (fst el),(snd el)))  (filter (\el -> (snd el)>=5) l))


{-32-}
myzip::[String]->[String]->[(String,String)]
myzip l1 l2 = aux_zip l1 l2 [] where
		aux_zip l1 l2 acc
			|null l1 = acc
			|otherwise = aux_zip (tail l1) (tail l2) (acc++[(head l1,head l2)])
