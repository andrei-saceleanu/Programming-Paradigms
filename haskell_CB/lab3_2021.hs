import Data.Char
import Data.Bits

--1.1
rem_upper = map (map toLower)

--1.2
longer::Int->[String]->[String]
longer x = filter ((<=x).length)

--1.3
howMany::[String]->Int
howMany = length.(filter ((>=12).length)) -- foldr (\x acc -> acc+1) 0

--1.4
names_emails::[String]->[[String]]
names_emails = foldr ((:).split_mail) [] --foldl (\acc x -> acc++[split_mail x]) [] 
	where
		aux_split s ac cg ch
			|null s = ac++[cg]
			|null cg = aux_split (tail s) ac (cg++[(head s)]) ch
			|(head s)==ch = aux_split (tail s) (ac++[cg]) [] ch
			|otherwise = aux_split (tail s) ac (cg++[(head s)]) ch
		split_mail s = aux_split s [] [] '@'


--1.5
my_elem::(Eq a)=>a->[a]->Bool
my_elem el l = foldl (\acc x -> if (x==el) then acc||True else acc||False ) False l


domains::[String]->[String]
domains l = foldl (\acc x->if my_elem x acc then acc else (acc++[x])) [] (map last (names_emails l))


--1.6
splitl::Char->String->[String]
splitl ch = reverse.(map reverse).(foldl op [[]])  -- foldr op' [[]]
	where
		op acc x  --op' x acc
			|x == ch = []:acc
			|otherwise = (x:(head acc)):(tail acc)


--1.7
splitBy::Char->String->[String]
splitBy ch str = aux_split str [] [] ch 
		where
			aux_split s ac cg ch
				|null s = ac++[cg]
				|null cg = aux_split (tail s) ac (cg++[(head s)]) ch
				|(head s)==ch = aux_split (tail s) (ac++[cg]) [] ch
				|otherwise = aux_split (tail s) ac (cg++[(head s)]) ch

domain::[String]->[String]
domain l = map head (map (splitBy '.') (map last (map (splitBy '@') l)))




s1 1 = True
s1 2 = True
s1 _ = False
--2.1
mem::(Integer->Bool)->Integer->Bool
mem = (\f x -> f x)


--2.2
spow2::Integer->Bool
spow2 = (\x -> (x>0)&&((x.&.(x-1))==0))


--2.3
snat::Integer->Bool
snat = (\x -> x>=0 && x==fromInteger (round x))

--2.4
intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection a b = (\x -> (a x)&&(b x))

--2.5
intersection' :: (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
intersection' a b x = (a x)&&(b x)


--2.6
toSet :: [Integer] -> (Integer -> Bool)
toSet l = (\x -> if elem x l then True else False)

--2.7
capList :: [Integer -> Bool] -> Integer -> Bool
capList l = foldl intersection (\x -> True) l
