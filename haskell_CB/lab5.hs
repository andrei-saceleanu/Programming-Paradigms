import Data.List
import Data.Char


data IntList = EmptyList | Cons Integer IntList

isum::IntList->Integer
isum EmptyList = 0
isum (Cons x xs) = x + isum xs

data List a = EmptyAList | Con a (List a) deriving Show

to_poly_list::IntList->List Integer
to_poly_list EmptyList = EmptyAList
to_poly_list (Cons x xs) = Con x (to_poly_list xs)

show_lists::(List a)->[a]
show_lists EmptyAList = []
show_lists (Con x xs) = x:(show_lists xs)

data Tree a = Void | Node (Tree a) a (Tree a) deriving Show

flatten :: List a -> [a]
flatten EmptyAList=[]
flatten (Con x xs)=[x]++(flatten xs)

to_poly::[a]->List a
to_poly [] = EmptyAList
to_poly (x:xs) = Con x (to_poly xs)

flattenT::Tree a->List a
flattenT Void = EmptyAList
flattenT (Node left x right) = to_poly ([x]++(flatten (flattenT left))++(flatten (flattenT right)))

app::(List a)->(List a)->(List a)
app l1 l2= to_poly ((show_lists l1)++(show_lists l2))

tmap::(a->b)->Tree a->Tree b
tmap f Void = Void
tmap f (Node left x right)=Node (tmap f left) (f x) (tmap f right)

tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith f Void Void = Void
tzipWith f (Node left1 x right1) (Node left2 y right2) = Node (tzipWith f left1 left2) (f x y) (tzipWith f right1 right2)

tfoldr::(a->b->b)->b->Tree a->b
tfoldr _ acc Void =acc
tfoldr f acc tree = foldr f acc (flatten (flattenT tree))


