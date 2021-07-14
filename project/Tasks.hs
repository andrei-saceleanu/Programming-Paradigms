{-
    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Text.Read
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}

strip_header:: Table -> Table
strip_header = tail

-- Task 1
compute_exam_grades :: Table -> Table
compute_exam_grades l = [["Nume","Punctaj Exam"]]++(map names_exam (tail l))
    where
        avg l = (foldl (+) 0 (map (\el -> if el/="" then read el::Float else 0) l))/4
        subpoints row =  init (tail row) 
        names_exam row = [(head row)]++[(printf "%.2f" ((avg (subpoints row))+(read (last row)::Float)))]

-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num = length.(filter (>=2.5)).(map (\el -> read (last el)::Float)).(tail).(compute_exam_grades)

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = (fromIntegral (get_passed_students_num table)::Float)/(fromIntegral ((length table)-1)::Float)

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg table = (foldl (+) 0 (map (\el -> read (last el)::Float) (tail (compute_exam_grades table))))/no_students
        where 
            no_students = fromIntegral ((length table)-1)::Float
            


-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num table = length (filter (>=1.5) (map (sum.toNumeric.get_homework_columns) (strip_header table)))
        where
            get_homework_columns l = take 3 (tail (tail l))
            toNumeric l = map (\el -> if el/="" then read el::Float else 0) l

-- Task 3

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs t = [["Q1","Q2","Q3","Q4","Q5","Q6"]]++[averages]
    where
        table_no_hdr_questions = map (init.tail) (strip_header t)
        table_len = fromIntegral (length table_no_hdr_questions)::Float
        numeric_table = map (map (\el -> if el/="" then read el::Float else 0)) table_no_hdr_questions
        sums_per_column = foldl (\acc x -> zipWith (+) acc x) [ 0.0| i<-[1..6]] numeric_table
        averages = map (printf "%.2f") (map (/table_len) sums_per_column)

-- Task 4
get_column::[[Int]]->Int->[Int]
get_column table index = map (\el -> el!!index) table 


get_exam_summary :: Table -> Table
get_exam_summary t = [["Q","0","1","2"]]++counts
    where
        table_no_hdr_questions = map (init.tail) (strip_header t)
        table_len = length table_no_hdr_questions
        numeric_table = map (map (\el -> if el/="" then read el::Int else 0)) table_no_hdr_questions
        get_count_of_value::[Int]->Int->Int
        get_count_of_value l val = length (filter (==val) l)
        get_012::[Int]->[String]
        get_012 l = map show ([(get_count_of_value l 0)]++[(get_count_of_value l 1)]++[(get_count_of_value l 2)])
        counts = [ ["Q"++(show (i+1))]++(get_012 (get_column numeric_table i))|i<-[0..5]]

-- Task 5
ins_sort::[a]->(a->a->Bool)->a->[a] --f == True daca x<y
ins_sort [] _ x = [x]
ins_sort (y:ys) f x = if (f x y)==True then x:y:ys else y:(ins_sort ys f x)

insertion_sort::[(String,Float)]->[(String,Float)]
insertion_sort [] = []
insertion_sort (x:xs) = ins_sort (insertion_sort xs) 
                        (\el1 el2 -> if (snd el1)/=(snd el2) then (snd el1)-(snd el2)<0 else (fst el1)<(fst el2)) x 

insertion_sort_v2::[([String],Float)]->[([String],Float)]
insertion_sort_v2 [] = []
insertion_sort_v2 (x:xs) = ins_sort (insertion_sort_v2 xs) 
                            (\el1 el2 -> if (snd el1)/=(snd el2) then (snd el1)-(snd el2)<0 else (head (fst el1))<(head (fst el2))) x 


get_ranking :: Table -> Table
get_ranking table = [["Nume","Punctaj Exam"]]++final_table
    where
        table_no_hdr = strip_header (compute_exam_grades table)
        tuple_table = map (\row -> (row!!0,read (row!!1)::Float)) table_no_hdr
        sorted_table = insertion_sort tuple_table
        final_table = map (\row -> [(fst row),(printf "%.2f" (snd row))]) sorted_table


-- Task 6
get_exam_diff_table :: Table -> Table
get_exam_diff_table table= [["Nume","Punctaj interviu","Punctaj scris","Diferenta"]]++final_table
    where
        table_no_hdr = strip_header table
        unsorted_table = map construct_entry table_no_hdr
        formatted_table = map (\el -> ((init el),read (last el)::Float)) unsorted_table
        sorted_table = insertion_sort_v2 formatted_table
        final_table = map (\el -> (fst el)++[(printf "%.2f" (snd el))]) sorted_table
        interview_questions l = init (tail l)
        interview_points l = (foldl (+) 0 (map (\el -> if el/="" then read el::Float else 0) l))/4
        int_points = interview_points.interview_questions
        exam_points row = read (last row)::Float
        construct_entry::Row->Row
        construct_entry row = (head row):(printf "%.2f" (int_points row)):(printf "%.2f" (exam_points row)):
                                (printf "%.2f" (abs ((int_points row)-(exam_points row)))):[]


{-
    TASK SET 2
-}

--split string by a given character separator
splitBy::Char->String->[String]
splitBy ch s = foldr op [""] s
        where op x acc
                |x==ch = "":acc --if current char is separator,make new string in accumulator
                |otherwise = (x:(head acc)):(tail acc) --else put it in first elem of acc

--reverse operation for splitBy
--takes a list of strings and concatenates the elements,with ch in between
concatWith::Char->[String]->String
concatWith ch l = foldr (\x acc -> x++[ch]++acc) "" l

--parse a string which represents a CSV file
--get the string rows by spliting after \n and then ,for each one,the list of fields
read_csv :: CSV -> Table
read_csv str = map (splitBy ',') (splitBy '\n' str) 

--get a table and construct the appropriate CSV string
--the use of init erases the extra separator appended at the end of one row
write_csv :: Table -> CSV
write_csv t = init (concatWith '\n' (map ((init).(concatWith ',')) t))

-- Task 1

--for a given list,return list of tuples (index,list element)
enumerate:: [a] -> [(Int,a)]
enumerate l = zip [i |i<-[0..((length l)-1)]] l

--return the index of a given element in a list
--if not found,the result is -1
find_index::(Eq a) => a -> [a] -> Int
find_index el l = foldl (\acc x -> if (snd x) == el then (fst x) else acc) (-1) (enumerate l)

--extract ,as a list of values,a certain column from a table
--find which index the given column corresponds to
--for every entry,get the element with the index found
as_list :: String -> Table -> [String]
as_list column_name t = map (\el -> el!!index) (tail t) 
    where 
        index = find_index column_name (head t)

-- Task 2

--check if string represents an integer
isInt :: String -> Bool
isInt str
    |(readMaybe str::Maybe Int) == Nothing = False
    |otherwise = True

--check if string represents a float
isFloat :: String -> Bool
isFloat str
    |(readMaybe str::Maybe Float) == Nothing = False
    |otherwise = True

--check if string is numeric(in this case,either integer of float)
isNumeric ::String -> Bool
isNumeric str = (isInt str)||(isFloat str)

--return an ordering value depending on the relation between given integer parameters
cmpInt :: Int -> Int ->Ordering
cmpInt x y
    |x < y = LT
    |x > y = GT
    |otherwise = EQ

--return an ordering value depending on the relation between given float parameters
cmpFloat:: Float -> Float ->Ordering
cmpFloat x y
    |x < y = LT
    |x > y = GT
    |otherwise = EQ

--return an ordering value depending on the relation between given string parameters
cmpString :: String -> String ->Ordering
cmpString x y
    |x < y = LT
    |x > y = GT
    |otherwise = EQ

--safe read of Int given as string
--if empty string,the result is 0
readBlankInt::String->Int
readBlankInt str = if str/="" then read str::Int else 0

--safe read of Float given as string
--if empty string,the result is 0
readBlankFloat::String->Float
readBlankFloat str = if str/="" then read str::Float else 0

--return an ordering value depending on the relation between given string parameters
--if both parameters are int or float,compare them accordingly(by value,not lexicographically)
cmpVal:: String -> String -> Ordering
cmpVal x y
    |(isInt x) && (isInt y) = cmpInt (readBlankInt x) (readBlankInt y)
    |(isFloat x) && (isFloat y) = cmpFloat (readBlankFloat x) (readBlankFloat y)
    |otherwise = cmpString x y


--sort the entries in a table by a given column
--the comparison is different depending on the type of values in that column
--if the values are the same,the comparison is performed depending on the values
--from first column
tsort :: String -> Table -> Table
tsort column_name t = (head t):(sortBy cmp (tail t))
    where
        index = find_index column_name (head t)
        cmp::Row->Row->Ordering
        cmp row1 row2 = if (row1!!index) /= (row2!!index) then cmpVal (row1!!index) (row2!!index) else cmpVal (row1!!0) (row2!!0)


-- Task 3

--apply a function to every value from a table
vmap :: (Value -> Value) -> Table -> Table
vmap f t = map (map f) t

-- Task 4

--apply a function to every row of a table and construct a new table with new column names
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f column_names t = column_names:(map f (tail t))

--for every entry(a student),form a new entry like [Name,Grade]
get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row):(printf "%.2f" grade):[]
    where
        grade = sum (map (\el -> if el/="" then read el::Float else 0) (tail (tail row)))

-- Task 5

--vertical union / stack the tables vertically
--if columns match,perform the merging
--else,keep only the first table
vunion :: Table -> Table -> Table
vunion t1 t2 = if (head t1)==(head t2) then t1++(tail t2) else t1

-- Task 6

--make a list of empty strings of given length k
white_spaces::Int->[String]
white_spaces k =  [""|i<-[1..k]]

--horizontal union of 2 tables
--in case of 2 tables with different lengths,the entries with no correspondent
--will be matched with empty strings (a list of length equal to the number of columns from the nonmatching table)
hunion :: Table -> Table -> Table
hunion table1 table2 = aux table1 table2 []
    where
        col_num1 = length (head table1) --number of columns from first table
        col_num2 = length (head table2) --number of columns from second table
        aux t1 t2 acc --auxiliary function(depending on the lengths of the tables,merge the entries as previously described)
            |null t1 && null t2 = acc
            |(null t1 == False) && null t2 = aux (tail t1) t2 (acc++[(head t1)++(white_spaces col_num2)])
            |null t1 && (null t2 == False) = aux t1 (tail t2) (acc++[(head t2)++(white_spaces col_num1)])
            |otherwise = aux (tail t1) (tail t2) (acc++[(head t1)++(head t2)])

-- Task 7

--given a "dictionary" of the form [(String,[String])] and a string key
--return the value associated to it(in this case,a list of strings)
find_match::String -> [(String,[String])]->[String]
find_match str [] = []
find_match str (y:ys) = if str == (fst y) then (snd y) else find_match str ys

--given 2 lists,find those pairs of indexes (index_list1,index_list2)
--which correspond to equal elements
--these pairs will be used to merge identical columns in a table join operation
merge::[String]->[String]->[(Int,Int)] --matching elems from l2(identified by index) override the pair from l1
merge l1 l2 = foldl (\acc x-> if elem (snd x) l1 then acc++[(find_index (snd x) l1,fst x)] else acc) [] (enumerate l2)

--given a "dictionary" of the form [(Int,Int)] and an integer key
--return the value associated to it(in this case,another integer)
find_tuple_match::Int->[(Int,Int)]->Int
find_tuple_match x [] = -1
find_tuple_match x (y:ys) = if x == (fst y) then (snd y) else find_tuple_match x ys

--table join on a given column
--[row!!i|i<-[0..(len-1)],i/=index]
tjoin :: String -> Table -> Table -> Table
tjoin column_name t1 t2 = final_table
    where
        index = find_index column_name (head t1) -- index of joining column in fst table
        index2 = find_index column_name (head t2) -- index of joining column in snd table
        len = length (head t1) --no of columns fst table
        len2 = length (head t2) -- no of columns snd table
        entries_t1 = map (\row -> (row!!index,((take index row)++(drop (index+1) row)))) t1 -- list of tuples with (key,[elements from columns other than joining one])
        entries_t2 = map (\row -> (row!!index,((take index2 row)++(drop (index2+1) row)))) t2 -- same for snd table
        keys2 = map fst entries_t2 --fst of every tuple fron entries_t2
        cols1 = snd (head entries_t1) --non joining columns from t1
        cols2 = snd (head entries_t2) -- same for t2
        merge_ops = merge cols1 cols2 --find the pairs of columns which might be merged (if a value is empty string,the override won't happen)
        source_of_merge = if merge_ops==[] then [] else map fst merge_ops --positions which should be overriden
        apply_ops l1 l2 = if merge_ops==[] then l1++l2 else map snd (map (\x -> if elem (fst x) source_of_merge && l2!!(find_tuple_match (fst x) merge_ops)/="" then (fst x,l2!!(find_tuple_match (fst x) merge_ops)) else x) (enumerate l1))
--no non-joining matching columns (merge_ops==[]) so just concatenate else override (*** empty strings dont override)
        op entry = ((fst entry),apply_ops (snd entry) (find_match (fst entry) entries_t2)) --join entries with same key
--for every tuple from table1,if there is no correspondent,just fill with empty strings,else apply the operations merge_ops if any
        c = map (\e -> if elem (fst e) keys2 == False then (fst e,(snd e)++(white_spaces (len2-1))) else op e) entries_t1
        final_table = map (\el -> [(fst el)]++(snd el)) c --restore the appropriate format of a table

-- Task 8

--cartesian product of 2 tables
--get one entry from t1 and another from t2,apply a merging function f
--and store the result entry in a new table
{-
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f column_names t1 t2 = column_names:(cart_prod f (tail t1) (tail t2))
    where
        cart_prod f t1 t2 = foldl (\acc x -> acc++(foldl (\a el -> a++[f x el]) [] t2)) [] t1
-}
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f column_names t1 t2 = column_names:(cart_prod f (tail t1) (tail t2))
    where
        cart_prod f t1 t2 = [ f x y |x<-t1,y<-t2]


-- Task 9

--extract certain columns from a table into a new one
--the indexes of those columns are found and stored in col_indexes
--for every entry in table t(a list of values),only the values with indexes in col_indexes
--will be kept
--op row = map snd (filter (\el -> elem (fst el) col_indexes) (enumerate row))
projection :: [String] -> Table -> Table
projection columns t = map op t
    where
        col_indexes = map (\x -> find_index x (head t)) columns
        op row = [(snd el) |el<-(enumerate row),elem (fst el) col_indexes == True]

{-
    TASK SET 3
-}
data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

 
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
    show (CSV c) = show c
    show (Table t) = write_csv t
    show (List l) = show l

class Eval a where
    eval :: a -> QResult
 
get_table :: QResult -> Table
get_table (Table t) = t
get_table _ = []

get_value :: Maybe Value -> Value
get_value Nothing = ""
get_value (Just x) = x

type FilterOp = Row -> Bool

instance Eval Query where
    eval (FromCSV str) = Table (read_csv str)
    eval (ToCSV query) = CSV (show (eval query))
    eval (AsList colname query) = List (as_list colname (get_table (eval query)))
    eval (Sort colname query) = Table (tsort colname (get_table (eval query)))
    eval (ValueMap op query) = Table (vmap op (get_table (eval query)))
    eval (RowMap op column_names query) = Table (rmap op column_names (get_table (eval query)))
    eval (VUnion q1 q2) = Table (vunion (get_table (eval q1)) (get_table (eval q2)))
    eval (HUnion q1 q2) = Table (hunion (get_table (eval q1)) (get_table (eval q2)))
    eval (TableJoin str q1 q2) = Table (tjoin str (get_table (eval q1)) (get_table (eval q2)))
    eval (Cartesian f l q1 q2) = Table (cartesian f l (get_table (eval q1)) (get_table (eval q2)))
    eval (Projection l q) = Table (projection l (get_table (eval q)))
    eval (Filter fc q) = Table ((head t):(filter (feval (head t) fc) (tail t))) where t = get_table (eval q)
    eval (Graph op q) = Table (foldl (\acc x -> acc++(foldl (\a y -> if elem [(min (head x) (head y)),(max (head x) (head y))] (map init acc) || x==y || op x y == Nothing then a else a++[[(min (head x) (head y)),(max (head x) (head y)),(get_value (op x y))]]) [] t)) [["From","To","Value"]] t)  where t = tail (get_table (eval q))


data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval cols (Eq colname ref) = \row -> readBlankFloat (row!!index) == ref where index = find_index colname cols
    feval cols (Lt colname ref) = \row -> readBlankFloat (row!!index) < ref where index = find_index colname cols
    feval cols (Gt colname ref) = \row -> readBlankFloat (row!!index) > ref where index = find_index colname cols
    feval cols (In colname l) = \row -> elem (readBlankFloat (row!!index)) l where index = find_index colname cols
    feval cols (FNot fc) = \row -> not ((feval cols fc) row)
    feval cols (FieldEq s1 s2) = \row -> (readBlankFloat (row!!index)) == (readBlankFloat (row!!index2))  where 
                                                                                index = find_index s1 cols
                                                                                index2 = find_index s2 cols

instance FEval String where
    feval cols (Eq colname ref) = \row -> (row!!index) == ref where index = find_index colname cols
    feval cols (Lt colname ref) = \row -> (row!!index) < ref where index = find_index colname cols
    feval cols (Gt colname ref) = \row -> (row!!index) > ref where index = find_index colname cols
    feval cols (In colname l) = \row -> elem (row!!index) l where index = find_index colname cols
    feval cols (FNot fc) = \row -> not ((feval cols fc) row)
    feval cols (FieldEq s1 s2) = \row -> (row!!index) == (row!!index2)  where 
                                                                            index = find_index s1 cols
                                                                            index2 = find_index s2 cols

similarities_query = Sort "Value" (Graph edge_op3 (FromCSV lecture_grades_csv))

edge_op3 r1 r2 = if (not (null (head r1))) && (not (null (head r2))) && dist>=5 then Just (show dist) else Nothing
    where dist = sum (zipWith (\x y -> if x==y then 1 else 0 ) (tail r1) (tail r2))


{-
    TASK SET 4
-}
editDist::String->String->Int
editDist s1 s2 = d m n
    where 
        m = length s1
        n = length s2
        a = listArray (1, m) s1
        b = listArray (1, n) s2
        d i 0 = i
        d 0 j = j
        d i j
          | a!i ==  b!j = dp!(i-1,j-1)
          | otherwise = (minimum [dp!(i-1,j),dp!(i,j-1),dp!(i-1,j-1)])+1
        dp = listArray bounds [d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))


--find minimum edit distance string from given list,relative to a given string
find_closest::String->[String]->String
find_closest s l = l!!ind
    where
        vals = [editDist s el |el<-l]
        ind = find_index (minimum vals) vals

correct_table :: String -> CSV -> CSV -> CSV
correct_table colname typo_csv ref_csv = write_csv new_table
    where
        initial_table = read_csv typo_csv
        t = as_list colname initial_table
        ref = as_list colname (read_csv ref_csv)
        modified_column = map (\x -> if elem x ref then x else find_closest x ref) t
        ind = find_index colname (head initial_table)
        --insert modified column in the result table
        new_table = [(head initial_table)]++zipWith (\x row-> (take ind row)++[x]++(drop (ind+1) row)) modified_column (tail initial_table)


compute_hw_grades :: Table -> Table
compute_hw_grades t = [["Nume","Punctaj Teme"]]++(map names_hw (tail t))
    where
        names_hw row = (head row):(printf "%.2f" (sum (map (\x -> if x/="" then read x::Float else 0) (tail row)))):[]

compute_lecture_grades::Table->Table
compute_lecture_grades t = [["Nume","Punctaj Curs"]]++(map names_lect (tail t))
    where
        len = length (tail (head t))
        s row = 2*(sum (map (\x -> if x/="" then read x::Float else 0) (tail row)))
        grade row = (s row)/(fromIntegral len::Float)
        names_lect row = (head row):(printf "%.2f" (grade row)):[]


--for a table with 2 fields,find the corresponding value from first column,given the one in the second
find_table_match::String -> Table -> String
find_table_match s [] = ""
find_table_match s (x:xs) = if s==(last x) then (head x) else (find_table_match s xs)

grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades t1 t2 t3 t4 = write_csv complete_table
    where
        corrected_t1 = read_csv (correct_table "Nume" t1 t2)
        exams = compute_exam_grades (read_csv t3)
        hws = compute_hw_grades (read_csv t2)
        lecture_grades = read_csv t4
        new_header = "Nume":(tail (head lecture_grades))
        lecture = new_header:(map (\row -> (find_table_match (head row) corrected_t1):(tail row)) (filter (\x -> (head x)/="") (tail lecture_grades)))
        final_lecture = compute_lecture_grades lecture
        partial = tsort "Nume" (tjoin "Nume" (tjoin "Nume" hws final_lecture) exams)
        total row = if (readBlankFloat (row!!1))+(readBlankFloat (row!!2))<2.5 || (readBlankFloat (row!!3))<2.5 then row++["4.00"] else row++[printf "%.2f" ((minimum [(readBlankFloat (row!!1))+(readBlankFloat (row!!2)),5])+(readBlankFloat (row!!3)))]
        complete_table = ((head partial)++["Punctaj Total"]):(map total (tail partial))


