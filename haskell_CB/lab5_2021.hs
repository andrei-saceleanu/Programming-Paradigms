--1

myCompare :: Integer -> Integer -> Ordering
myCompare x y
    |x > y = GT
    |x < y = LT
    |otherwise = EQ

--2
insert :: Integer -> [Integer] -> [Integer]
insert x [] = [x]
insert x (y:ys) = if x < y then x:y:ys else y:(insert x ys)

--3
insertSort :: [Integer] -> [Integer] -- foldl insert [] l
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

--4
insertBy :: (Integer -> Integer -> Ordering)->Integer->[Integer]->[Integer]
insertBy _ x [] = [x]
insertBy f x (y:ys) = if (f x y) == LT then x:y:ys else y:(insertBy f x ys)

--5
insertSortBy :: (Integer -> Integer -> Ordering) -> [Integer] -> [Integer]
insertSortBy _ [] = []
insertSortBy f (x:xs) = insertBy f x (insertSortBy f xs)

data Point = Point Float Float deriving Show

pointToPair :: Point -> (Float, Float)
pointToPair (Point x y) = (x, y)

--6
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

--7
collinear :: Point -> Point -> Point -> Bool
collinear (Point x1 y1) (Point x2 y2) (Point x3 y3) = (x1*y2 + x2*y3 + x3*y1 - x3*y2 - y3*x1 - x2*y1) == 0

data Natural = Zero | Succ Natural deriving Show

--8
add :: Natural -> Natural -> Natural
add x Zero = x
add Zero x = x
add (Succ x) (Succ y) = Succ (Succ (add x y))

mul :: Natural -> Natural -> Natural
mul _ Zero = Zero
mul Zero _ = Zero
mul (Succ x) (Succ y) = Succ (add (add (mul x y) x) y)

data IList = IVoid | ICons Int IList
data List a = Void | Cons a (List a)

--10
myLength :: List a -> Integer
myLength Void = 0
myLength (Cons x xs) = 1 + (myLength xs)


--11
toHaskell :: List a -> [a]
toHaskell Void = []
toHaskell (Cons x xs) = x:(toHaskell xs)

--12
data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

--13
height :: Tree a -> Integer
height Empty = 0
height (Node l x r) = 1 + (max (height l) (height r))

--14
size :: Tree a -> Integer
size Empty = 0
size (Node l x r) = 1 + (size l) + (size r)


--15
mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node l x r) = Node (mirror r) x (mirror l)

--16
treeMap :: (a->b) -> Tree a -> Tree b
treeMap _ Empty = Empty
treeMap f (Node l x r) = Node (treeMap f l) (f x) (treeMap f r)


--17
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l x r) = [x] ++ (flatten l) ++ (flatten r)

data Student = Student String String [Float]


--18
avg :: Student -> Float
avg (Student fname lname grades) = (foldl (+) 0 grades)/(fromIntegral (length grades)::Float)

--19
studComp :: Student -> Student -> Ordering
studComp s1 s2
    |avg s1 < avg s2 = LT
    |avg s1 > avg s2 = GT
    |otherwise = EQ

--20
get_full_name:: Student -> String
get_full_name (Student fname lname grades) = fname++" "++lname

highestAverage :: [Student] -> String
highestAverage l = get_full_name (foldl (\acc x -> if studComp x acc == GT then x else acc) (Student "" "" [0]) l)

data AExpr = Const Int | Var String | Add AExpr AExpr | Mul AExpr AExpr
data BExpr = Eq AExpr AExpr | Not BExpr | Gt AExpr AExpr
type Context = [(String, Int)]

--21
search :: Context -> String -> Int
search [] _ = 0
search ((x,y):xs) key = if key==x then y else search xs key

--22
evalA :: Context -> AExpr -> Int
evalA c (Const x) = x
evalA c (Var x) = search c x
evalA c (Add a b) = (evalA c a) + (evalA c b)
evalA c (Mul a b) = (evalA c a) * (evalA c b)

--23
evalB :: Context -> BExpr -> Bool
evalB c (Eq a b) = (evalA c a) == (evalA c b)
evalB c (Not be) = not (evalB c be)
evalB c (Gt a b) = (evalA c a) > (evalA c b)