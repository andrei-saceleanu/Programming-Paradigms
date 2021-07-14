import Dataset
import Data.List

splitBy::Char->String->[String]
splitBy ch s = foldr op [] s
        where op x acc
                |x==ch = []:acc
                |null acc = [[x]]
                |otherwise = (x:(head acc)):(tail acc)

concatWith::Char->[String]->String
concatWith ch l = init (foldr (\x acc -> x++[ch]++acc) "" l)

parseit::String -> String
parseit str = concatWith '\n' (map (concatWith ',') sorted)
    where 
        sname str = concatWith ' ' (sort (splitBy ' ' str))
        sorted_names = map (\x -> (sname (head x)):(tail x)) (map (splitBy ',') (splitBy '\n' str ))
        sorted = sortBy (\x y -> if (head x)<(head y) then LT else if (head x)>(head y) then GT else EQ) sorted_names

mygroup::String->[(String,[String])]
mygroup str = tail (aux [] ("",[]) sorted_str)
    where
        sorted_str = map (splitBy ',') (splitBy '\n' (parseit str))
        aux acc c_group l
            |null l = acc++[c_group]
            |(head (head l)) == (fst c_group) = aux acc (fst c_group,(snd c_group)++(tail (head l))) (tail l)
            |otherwise = aux (acc++[c_group]) ((head (head l)),(tail (head l))) (tail l)


check_dup::String -> Bool
check_dup str = aux [] False names
    where 
        names = map (head.(splitBy ',')) (tail (splitBy '\n' str))
        aux acc res l
            |null l = res
            |elem (head l) acc = True
            |otherwise = aux ((head l):acc) res (tail l) 
