data IntList = IVoid | Cons Integer IntList

isum :: IntList -> Integer
isum IVoid = 0
isum (Cons x xs) = x + (isum xs)

data List a = LVoid | LCons a (List a) deriving Show

to_poly_list :: IntList -> List Integer
to_poly_list IVoid = LVoid
to_poly_list (Cons x xs) = LCons x (to_poly_list xs)

show_list :: List a -> [a]
show_list LVoid = []
show_list (LCons x xs) = x:(show_list xs)

data Tree a = Void | Node (Tree a) a (Tree a) deriving Show

my_tree = Node (Node Void 2 Void) 1 (Node Void 3 Void)

list_append::List a->List a->List a
list_append LVoid l = l
list_append (LCons x xs) l = LCons x (list_append xs l)

flatten :: Tree a -> List a
flatten Void = LVoid
flatten (Node l x r) = LCons x (list_append (flatten l) (flatten r))

tmap::(a->b)->Tree a->Tree b
tmap _ Void = Void
tmap f (Node l x r) = Node (tmap f l) (f x) (tmap f r)

tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith f (Node l x r) (Node l2 x2 r2) = Node (tzipWith f l l2) (f x x2) (tzipWith f r r2)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Void = acc
tfoldr f acc (Node l x r) = f x (tfoldr f (tfoldr f acc l) r)

tflatten::Tree a->List a
tflatten = tfoldr LCons LVoid

data Extended = Infinity | Value Integer deriving Show
extSum :: Extended -> Extended -> Extended
extSum Infinity _ = Infinity
extSum _ Infinity = Infinity
extSum (Value x) (Value y) = Value (x+y)

equal :: Extended -> Extended -> Bool
equal Infinity Infinity = True
equal Infinity _ = False
equal _ Infinity = False
equal (Value x) (Value y) = x==y

lhead :: List a -> Maybe a
lhead LVoid = Nothing
lhead (LCons x xs) = Just x

ltail :: List a -> Maybe (List a)
ltail LVoid = Nothing
ltail (LCons x xs) = Just xs