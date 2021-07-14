{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



import Data.Char
import qualified Data.List as L

-- Data types
data List a = Null | Cons a (List a)
data BTree a = Void | Node a (BTree a) (BTree a)
data Student = Student {
  first_name :: String,
  last_name  :: String,
  grades     :: [Float]
}
{--
  For Student, we can define it as a 'struct Student' from C.
  first_name will be a "getter" for our first String.
  Haskell helps us by defining 3 simple functions for us to extract each member
  Try :t Student and :t first_name in GHCI and you'll figure it out quickly.
--}


-- We should also have some test data You can also create your own.
tree1 = Node 1 (Node 2 (Node 3 Void Void) Void) (Node 4 Void (Node 5 Void Void))
tree2 = Node 2 (Node 3 (Node 1 Void Void) Void) (Node 5 Void (Node 4 Void Void))  -- same as tree1, but different nodes order
tree3 = Node 1 (Node 2 (Node 3 (Node 7 Void Void) (Node 10 Void Void)) (Node 4 (Node 6 Void Void) (Node 9 Void Void))) (Node 5 (Node 8 Void Void) Void)
tree4 = Node 1.2 (Node 3.4 Void (Node 1.4 Void Void)) (Node 3.0 Void Void) -- and a float tree


list1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 Null)))
list2 = Cons 3 (Cons 4 (Cons 1 (Cons 2 Null)))
-- list3 = Cons list1 list2

student1 = Student "Alex" "Andrei" [4.2,3.0,2.3]
student2 = Student "Matei" "Popovici" [10.0,7.7,9.4]
student3 = Student "Mihai" "Dumitru" [7.7,9.4,10.0]
student4 = Student "Alex" "Dumitru" [7.7,9.4,10.0]
students = [student1, student2, student3]


-- 0. Adding 'deriving Show' to our data types will print them in a 'default' manner (you can try).
-- Let's print a tree in our way. For this we will enroll BTree in the Show class

-- Because we'll have to also print the value encapsulated in each node,
-- the value must also be "showable". So we will have to add this restriction on 'a'.

-- instance (Show a) => Show (BTree a) where
--   show Void         = ""
--   show (Node v l r) = "<"++(show l)++(show v)++(show r)++">"


-- It's nice, but let's do this more "stylish", just for flexing. We'll create the next showTree function
data Side = LeftSide | RightSide deriving Eq
data Front = Lane | Tab deriving Eq

generateLane :: String
generateLane = "─" ++ (replicate 3 ' ')

generateTab :: String
generateTab = " " ++ (replicate 3 ' ')

generateValue :: (Show a) => BTree a -> Side -> String
generateValue Void childSide =
  (if childSide == LeftSide then "┌" else "┐") ++ (replicate 3 '│') ++ "@" ++ "\n"
generateValue (Node value _ _) childSide =
  (if childSide == LeftSide then "┌" else "┐") ++ (replicate 3 '│') ++ (show value) ++ "\n"

generateNodeText :: (Show a) => [Front] -> Side -> BTree a -> String
generateNodeText fronts childSide node =
  (concat $ map (\frontType -> if frontType == Lane then generateLane else generateTab) fronts)
  ++ (generateValue node childSide)

showTree :: (Show a) => [Front] -> Side -> BTree a -> String
showTree fronts childSide tree@(Void) = generateNodeText fronts childSide tree
showTree fronts childSide node@(Node value left right) =
  if childSide == LeftSide then
    showTree (fronts ++ [Tab]) LeftSide left ++
    generateNodeText fronts childSide node ++
    showTree (fronts ++ [Lane]) RightSide right
  else
    showTree (fronts ++ [Lane]) LeftSide left ++
    generateNodeText fronts childSide node ++
    showTree (fronts ++ [Tab]) RightSide right

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)

splitByR sep string = foldr (\char acc@(currentStr:ls) -> if char == sep then [] : acc else (char : currentStr) : ls) [[]] string


instance (Show a) => Show (BTree a) where
  show Void = ")("
  show tree@(Node value left right) =
    let res = splitByR '\n' $ (showTree [] LeftSide left) ++ (show value) ++ "\n" ++ (showTree [] RightSide right)
    in
      concat $ map (\line -> line ++ "\n") $
      transpose $
      map (\line -> take (max (length res) (maximum $ map length res)) $ line ++ repeat ' ') res


{-

  1. Add List and Student to the Show class. You can print them however you want.
  If you aren't inspired today, you can use the following:
    *. The lists can be the default style [1,2,3].
    *. The student can be something like -> "Studentul: ANDREI Alex-Bogdan = [8.5,6.0,8.7]"
-}
instance (Show a)=>Show (List a) where
    show Null = "[]"
    show (Cons x Null) = (show x)
    show (Cons x xs) = (show x)++","++(show xs)

instance Show Student where
    show s = "Studentul: "++ (map toUpper (last_name s))++" "++(first_name s)++" = "++(show (grades s))



{--
  2. The default '==' that we get from 'deriving Eq' will check if 2 objects are identical.
  For our data, you'll have to provide a custom '==' such that:
    - list1 == list2 = True if both trees have the same elements, but in any order
    - tree1 == tree2 = True if both trees have the same elements, but in any order
    - stud1 == stud2 = True if both students have the same average on their grades
--}
list_len::List a->Int
list_len Null = 0
list_len (Cons x xs) = 1 + (list_len xs)

tree_size::(BTree a)->Int
tree_size Void = 0
tree_size (Node x l r) = 1 + (tree_size l) + (tree_size r)

to_haskell::(List a)->[a]
to_haskell Null = []
to_haskell (Cons x xs) = x:(to_haskell xs)

flatten :: BTree a -> [a]
flatten Void = []
flatten (Node x l r) = [x] ++ (flatten l) ++ (flatten r)

avg::[Float]->Float
avg l = sum l / (fromIntegral (length l))


instance (Eq a,Ord a)=>Eq (List a) where
    Null == Null = True
    (Cons x xs) == (Cons y ys) = L.sort (to_haskell (Cons x xs)) == L.sort (to_haskell (Cons y ys))
    _ == _ = False

instance (Eq a,Ord a)=>Eq (BTree a) where
    Void == Void = True
    (Node x l r) == (Node y l2 r2) = L.sort (flatten (Node x l r)) == L.sort (flatten (Node y l2 r2))
    _ == _ = False

instance Eq Student where
    (Student f l g) == (Student f2 l2 g2) = avg g == avg g2 --(f==f2) && (l==l2) && (g==g2)


{--
  3. We would like to use + and * on lists and trees to add/multiply corespondent elements.
  Enroll BTree and List in the Num class to access the + and * functions.
  If 2 lists or trees aren't at the same size, then we should consider unexisting corespondents as Null/Void.
  Also:
    Void + Node = Node
    Void * Node = Void
    same for lists
--}
instance (Num a)=>Num (List a) where
    Null + Null = Null
    Null + (Cons x xs) = (Cons x (xs+Null))
    (Cons x xs) + Null = (Cons x (xs+Null))
    (Cons x xs) + (Cons y ys) = (Cons (x+y) (xs+ys))
    Null * Null = Null
    Null * (Cons x xs) = (Cons x (xs*Null))
    (Cons x xs) * Null = (Cons x (xs*Null))
    (Cons x xs) * (Cons y ys) = (Cons (x*y) (xs*ys))

instance (Num a)=>Num (BTree a) where
    Void + Void = Void
    Void + (Node x l r) = (Node x (l+Void) (r+Void))
    (Node x l r) + Void = (Node x (l+Void) (r+Void))
    (Node x l r) + (Node y l2 r2) = (Node (x+y) (l+l2) (r+r2))
    Void * Void = Void
    Void * (Node x l r) = Void
    (Node x l r) * Void = Void
    (Node x l r) * (Node y l2 r2) = (Node (x*y) (l*l2) (r*r2))
    


{--
  4. Let's sort students now. Add the student to the Ord class and provide implementations for <.
  The criteria will be their grades average, then maximum grade, last_name and first_name alphabetical.

  We will sort them by rankings, stud1 < stud2 if stud1 is better than stud2 by the above criteria.
--}


instance Ord Student where
    s1 < s2 = if (avg (grades s1))<(avg (grades s2)) then True else
            if (maximum (grades s1))<(maximum (grades s2)) then True else
            if (last_name s1)<(last_name s2) then True else
            if (first_name s1)<(first_name s2) then True else False 



{--
  What if we need to create our own classes?
  For a quick example, we would like a class that tells us if a data type is Empty or not.
  We will call this class IsVoid and all types enrolled in this class must implement the isVoid method.

  As you can see, now we can add our data types in the new class, but also Haskell's types.
  The only requirement is that the enrolled type must implement our method.
--}

class IsVoid a where
  isVoid :: a -> Bool

instance IsVoid Bool where
  isVoid False = True
  isVoid True  = False


instance IsVoid (BTree a) where
  isVoid Void = True
  isVoid _    = False

instance IsVoid [a] where
  isVoid [] = True
  isVoid _  = False


{-
  5. Create a class 'Contains b a' that will require a 'contains :: b -> a -> Bool' method which will return True if 'a' is in 'b'.
  Do we need any additional restrictions for a or b? You can still add restrictions
    'class (SomeClass a) => Contains b a where ...'
-}
data HaskellList a = HL [a] 

class (Eq a)=>Contains b a where
    contains :: b -> a -> Bool

instance (Eq a)=>Contains (HaskellList a) a where
    contains (HL l) el = elem el l

instance (Eq a)=>Contains [a] a where
    contains l el = elem el l

instance (Eq a)=>Contains (List a) a where
    contains l el = elem el (to_haskell l)

instance (Eq a)=>Contains (BTree a) a where
    contains t el = elem el (flatten t)
{-
  6. Create the 'class Size a' which will require the methods 'size' and 'uniqueSize'
     - size = the numbers of elements in 'a'
     - uniqueSize = the number of uniqueElements in 'a'
-}


class Size a where
    size :: a -> Int
    uniqueSize :: a -> Int

instance (Eq a)=>Size (HaskellList a) where
    size (HL l) = length l
    uniqueSize (HL l) = length (foldl (\acc x -> if elem x acc then acc else x:acc) [] l)

instance (Eq a) => Size [a] where
  size l = length l
  uniqueSize l = length (foldl (\acc x -> if elem x acc then acc else x:acc) [] l)

instance (Eq a)=>Size (List a) where
    size = list_len
    uniqueSize l = length (foldl (\acc x -> if elem x acc then acc else x:acc) [] (to_haskell l))


instance (Eq a)=>Size (BTree a) where
    size = tree_size
    uniqueSize t = length (foldl (\acc x -> if elem x acc then acc else x:acc) [] (flatten t))
