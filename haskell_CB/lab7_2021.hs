

--1
data Tree a = Empty | Node a [Tree a] deriving Show
data BTree a = Void | BNode a (BTree a) (BTree a)

t = Node 1 [(Node 2 []),(Node 3 [(Node 5 []),(Node 2 []),(Node 7 [])]),(Node 4 [])]
t2 = Node 1 [(Node 2 []),(Node 7 [(Node 5 []),(Node 5 []),(Node 7 [])]),(Node 2 [])]

--2
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node k children) = Node (f k) (fmap (fmap f) children)

--3
instance Foldable Tree where
    foldr f acc Empty = acc
    foldr f acc (Node k children) = f k (foldr (\el a ->foldr f a el) acc children)

--4
class Zippable t where
    zipp :: (a->b->c) -> t a -> t b -> t c

--5
instance Zippable [] where
    zipp f _ [] = []
    zipp f [] _ = []
    zipp f l1 l2 = (f (head l1) (head l2)):(zipp f (tail l1) (tail l2))

--6
instance Zippable Maybe where
    zipp f Nothing _ = Nothing
    zipp f _ Nothing = Nothing
    zipp f (Just x) (Just y) = Just (f x y)

--7
instance Zippable (Either a) where
    zipp _ (Left a) _ = Left a
    zipp _ _ (Left a) = Left a
    zipp f (Right x) (Right y) = Right (f x y)

--8
instance Zippable BTree where
    zipp f _ Void = Void
    zipp f Void _ = Void
    zipp f (BNode k l r) (BNode k2 l2 r2) = BNode (f k k2) (zipp f l l2) (zipp f r r2)

--9
instance Zippable Tree where
    zipp f _ Empty = Empty
    zipp f Empty _ = Empty
    zipp f (Node k l) (Node k2 l2) = Node (f k k2) (zipWith (\x y -> zipp f x y) l l2)

--10
instance Zippable ((,) a) where
    zipp f (_,x) (n,y) = (n,f x y)

--11
instance Zippable ((->) d) where
    zipp f x y= \m -> f (x m) (y m)