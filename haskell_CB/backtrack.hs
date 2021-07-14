subsets::[a]->[[a]]
subsets [] = [[]]
subsets (x:xs) = (subsets xs)++(map (x:) (subsets xs))

insert_elem::a->[a]->[[a]]
insert_elem e lst = [(take i lst)++[e]++(drop i lst)|i<-[0..l]]where l = length lst

permutations::[a]->[[a]]
permutations [x] = [[x]]
permutations (x:xs) = foldl (++) [] (map (insert_elem x) (permutations xs))


combinations::[a]->Int->[[a]]
combinations [] _ = [[]]
combinations (x:xs) n  
		|length (x:xs) == n = [(x:xs)]
		|n==1 =[[el] |el<-(x:xs)]
		|otherwise = (map (x:) (combinations xs (n-1)))++(combinations xs n)
