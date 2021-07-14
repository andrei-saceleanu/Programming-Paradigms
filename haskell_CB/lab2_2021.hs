import Data.Char

--2.1.1

nr_elements::[a]->Int
nr_elements [] = 0
nr_elements (x:xs) = 1+(nr_elements xs)

--2.1.2
concat_lists::[[a]]->[a]
concat_lists [] = []
concat_lists (x:xs) = x++(concat_lists xs)

--2.1.3
my_append::[a]->[a]->[a]
my_append [] l2 = l2
my_append (x:xs) l2 = x:(my_append xs l2)

concat_lists2::[[a]]->[a]
concat_lists2 [] = []
concat_lists2 (x:xs) = my_append x (concat_lists xs)

concatIntCons :: [[Integer]] -> [Integer] 
concatIntCons [] = [] 
concatIntCons ([]:xs) = concatIntCons xs 
concatIntCons ((y:x):xs) = y : concatIntCons (x:xs)

--2.1.4
unique::Eq a=>[a]->[a]
unique l = aux l []
			where
				aux l res
					|null l = res
					|elem (head l) res = aux (tail l) res
					|otherwise = aux (tail l) (res++[(head l)])

g (x:y:z:w:l) = w

--2.3.1

firstUpper::[String]->[String]
firstUpper [] = []
firstUpper (x:xs) = if null x then [x]++(firstUpper xs) else [new_first]++(firstUpper xs)
					where
						new_first = if (ord (head x))>=97 then (chr (ord (head x)-32)):(tail x) else x 


--2.3.2
allUpper::[String]->[String]
allUpper [] = []
allUpper (x:xs) = [toUpper ch|ch<-x]:(allUpper xs)


--2.3.3
search::String->String->Int
search txt pat = if (length txt) < (length pat) then 0 else aux txt pat 0
			where
				aux txt pat occ
					|(length txt) < (length pat) = occ
					|take (length pat) txt == pat = aux (tail txt) pat (occ+1)
					|otherwise = aux (tail txt) pat occ


--2.3.4
startsWith::[String]->Char->[String]
startsWith [] ch = []
startsWith (x:xs) ch = if (head x)==ch then x:(startsWith xs ch) else startsWith xs ch



f::String->[(String,[String])]->[String]
f group dict = aux group dict []
			where
				aux group dict res
					|null dict = res
					|(fst (head dict))==group = aux group (tail dict) (res++(startsWith (snd (head dict)) 'M'))
					|otherwise = aux group (tail dict) res



--2.3.5
f5::[String]->[String]->[(String,String)]
f5 lst1 lst2 = [(lst1!!i,lst2!!i)|i<-[0..(l-1)]] where l = length lst1

--2.3.6
f6::[String]->[String]->[String]
f6 lst1 lst2 = [(head (lst1!!i)):(lst2!!i) |i<-[0..(l-1)]] where l = length lst1

--2.3.7
f7::[(String,Int)]->[([String],Int)]
f7 l = map (\el -> if (length (words (fst el)))==3 then (words (fst el),(snd el)+1) else (words (fst el),(snd el)) ) (filter (\el-> (snd el)>=5) l)
