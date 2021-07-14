import Data.List
import Data.Ord

compress::Eq a=>[a]->[a]
compress l = aux l [] 
	where
		aux l acc
			|null l = acc
			|null acc = aux (tail l) (acc++[(head l)])
			|(head l)/=(last acc) = aux (tail l) (acc++[(head l)])
			|otherwise = aux (tail l) acc


pack::Eq a=>[a]->[[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:y:xs) = let next = pack (y:xs) in
				if x/=y then [x]:next else ([x]++(head next)):(tail next)


encode::Eq a=>[a]->[(a,Int)]
encode l = aux l [] [] 0
	where
		aux l acc c_group curr_len
			|null l = acc++[(head c_group,length c_group)]
			|null c_group = aux (tail l) acc (c_group++[(head l)]) (curr_len+1)
			|(head l)/=(last c_group) = aux (tail l) (acc++[(head c_group,length c_group)]) [(head l)] 1
			|otherwise = aux (tail l) acc (c_group++[(head l)]) (curr_len+1)

repli::[a]->Int->[a]
repli [] _ = []
repli (x:xs) n = [ x |i<-[1..n]]++(repli xs n)


dropEvery::[a]->Int->[a]
dropEvery l n = [l!!(i-1) |i<-[1..(length l)],mod i 3 /= 0]

slice::[a]->Int->Int->[a]
slice [] _ _ = []
slice (x:xs) l r  
	|l==r = [x]
	|l==1 = x:(slice xs l (r-1))
	|otherwise = slice xs (l-1) (r-1)


range::Int->Int->[Int]
range a b
	|a==b = [a]
	|otherwise = a:(range (a+1) b)

graycode::Int->[String]
graycode n = if n==1 then ["0","1"] else  (map ('0':) prev)++(map ('1':) (reverse prev)) where prev = graycode (n-1)

{--
find_free_sep::String->Int
find_free_sep s = aux s 0 0
		where
			aux s pos open_brackets
				|null s = -1
				|(head s)=='(' = aux (tail s) (pos+1) (open_brackets+1)
				|(head s)==')' = aux (tail s) (pos+1) (open_brackets-1)
				|(head s)=='$' = if open_brackets==0 then pos else aux (tail s) (pos+1) open_brackets
				|otherwise = aux (tail s) (pos+1) open_brackets


huffman_aux::[(String,Int)]->String
huffman_aux l = if length l == 1 then (fst (l!!0)) 
			else huffman_aux (("("++(fst (min2!!0))++"$"++(fst (min2!!1))++")",(sum (map snd min2))):rest)
			where
				sorted_list = sortBy (\a b -> if (snd a)<(snd b) then LT else GT) l
				min2 =take 2 sorted_list
				rest = drop 2 sorted_list

deserialize str = aux str "" []
	where
		aux str code acc
			|length str == 1 = acc++[(str,code)]
			|head str == '(' = 	aux (drop ((find_free_sep (init (tail str)))+1) str) (code++"1") acc
			|otherwise = aux (drop ((find_free_sep str)+1) str) (code++"1") acc


--huffman::[(String,Int)]->[(String,String)]
huffman l = deserialize serialized where serialized = huffman_aux l
--}


data HTree a = Leaf a |Branch (HTree a) (HTree a) deriving Show  --Huffman Tree


huffman::Ord a=>[(a,Int)]->[(a,String)]
huffman freq = sortBy (comparing fst) $ serialize $ htree $ sortBy (comparing fst) [(w,Leaf x) |(x,w)<-freq]
	where
		htree [(_,t)] = t
		htree ((w1,t1):(w2,t2):ws) = htree $ insertBy (comparing fst) (w1+w2,Branch t1 t2) ws 
		serialize (Leaf x) = [(x,"")]
		serialize (Branch l r) = [(x,'0':code)| (x,code)<- serialize l]++[(x,'1':code)| (x,code)<- serialize r]


data BinTree a = Empty |Node a (BinTree a) (BinTree a) deriving (Show,Eq)  --BST

constructTree::Ord a=>[a]->BinTree a
constructTree l
	|length l == 0 = Empty
	|length l == 1 = Node (head l) Empty Empty
 	|otherwise = Node (s!!len2) (constructTree (take len2 s)) (constructTree (drop (len2+1) s))
	where
		len = length l
		len2 = div len 2
		s = sort l

insertTree::Ord a=>BinTree a->a->BinTree a
insertTree Empty x = Node x Empty Empty
insertTree (Node a l r) x = if x < a then Node a (insertTree l x) r else Node a l (insertTree r x)

--Directed graph
paths :: Eq a =>a -> a -> [(a,a)] -> [[a]] 
paths source sink edges 
    | source == sink = [[sink]]
    | otherwise = [
        source:path | edge<-edges, (fst edge) == source,
        path<-(paths (snd edge) sink [e |e<-edges, e/=edge])
    ]

type Graph = ([Int],[(Int,Int)]) -- vertices & edge list



dfs::Graph->Int->[Int]
dfs (v,e) n
	|[x|x<-v,x==n] == [] = []
	|otherwise = dfs_aux (v,e) [n]


dfs_aux::Graph->[Int]->[Int]
dfs_aux ([],_) _ = []
dfs_aux (_,_) [] = []
dfs_aux (v,e) (top:stack)
	|[x |x<-v,x==top] == [] = dfs_aux (new_v,e) stack
	|otherwise = top:(dfs_aux (new_v,e) (adjacent++stack))
	where
		adjacent = [x |(x,y)<-e,y==top] ++ [x |(y,x)<-e,y==top]
		new_v = [x |x<-v,x/=top]






