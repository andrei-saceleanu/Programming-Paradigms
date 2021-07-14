type Graph=[(Int,[Int])]

get_adj::Graph->Int->[Int]
get_adj (e:l) a = if a==(fst e) then (snd e) else get_adj l a

get_unviz_neighbors::Graph->[Int]->(Int,Int)->[(Int,Int)]
get_unviz_neighbors g viz node = [(el,(snd node)+1)|el<-(get_adj g (fst node)),not (elem el viz)]

bfs::Graph->Int->[(Int,Int)]
bfs g s = bfs_aux g [(s,0)] [s] [] where
	bfs_aux g queue viz res
		|null queue = res
		|otherwise = bfs_aux g ((tail queue)++(get_unviz_neighbors g viz (head queue))) (viz++(map fst (get_unviz_neighbors g viz (head queue)))) (res++[(head queue)])


my_reverse::[Integer]->[Integer]
my_reverse [] = []
my_reverse (e:l) = (my_reverse l)++[e]


main::IO()
main = do
	let g=[(1,[2,3]),(2,[1,4,5]),(3,[1,6,7]),(4,[2]),(5,[2]),(6,[3]),(7,[3])]
	print (bfs g 1)
	return ()

